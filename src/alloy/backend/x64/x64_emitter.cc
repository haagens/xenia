/**
 ******************************************************************************
 * Xenia : Xbox 360 Emulator Research Project                                 *
 ******************************************************************************
 * Copyright 2013 Ben Vanik. All rights reserved.                             *
 * Released under the BSD license - see LICENSE in the root for more details. *
 ******************************************************************************
 */

#include <alloy/backend/x64/x64_emitter.h>

#include <alloy/backend/x64/x64_backend.h>
#include <alloy/backend/x64/x64_code_cache.h>
#include <alloy/backend/x64/x64_function.h>
#include <alloy/backend/x64/x64_sequences.h>
#include <alloy/backend/x64/x64_thunk_emitter.h>
#include <alloy/hir/hir_builder.h>
#include <alloy/runtime/debug_info.h>
#include <alloy/runtime/runtime.h>
#include <alloy/runtime/symbol_info.h>
#include <alloy/runtime/thread_state.h>

using namespace alloy;
using namespace alloy::backend;
using namespace alloy::backend::x64;
using namespace alloy::hir;
using namespace alloy::runtime;

using namespace Xbyak;


namespace alloy {
namespace backend {
namespace x64 {

static const size_t MAX_CODE_SIZE = 1 * 1024 * 1024;

static const size_t STASH_OFFSET = 32;

// If we are running with tracing on we have to store the EFLAGS in the stack,
// otherwise our calls out to C to print will clear it before DID_CARRY/etc
// can get the value.
#define STORE_EFLAGS 1

}  // namespace x64
}  // namespace backend
}  // namespace alloy


const uint32_t X64Emitter::gpr_reg_map_[X64Emitter::GPR_COUNT] = {
  Operand::RBX,
  Operand::R12, Operand::R13, Operand::R14, Operand::R15,
};

const uint32_t X64Emitter::xmm_reg_map_[X64Emitter::XMM_COUNT] = {
  6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
};


X64Emitter::X64Emitter(X64Backend* backend, XbyakAllocator* allocator) :
    runtime_(backend->runtime()),
    backend_(backend),
    code_cache_(backend->code_cache()),
    allocator_(allocator),
    current_instr_(0),
    CodeGenerator(MAX_CODE_SIZE, AutoGrow, allocator) {
}

X64Emitter::~X64Emitter() {
}

int X64Emitter::Initialize() {
  return 0;
}

int X64Emitter::Emit(
    HIRBuilder* builder,
    uint32_t debug_info_flags, runtime::DebugInfo* debug_info,
    void*& out_code_address, size_t& out_code_size) {
  // Reset.
  if (debug_info_flags & DEBUG_INFO_SOURCE_MAP) {
    source_map_count_ = 0;
    source_map_arena_.Reset();
  }

  fprintf(stdout, debug_info->hir_disasm());

  // Fill the generator with code.
  size_t stack_size = 0;
  int result = Emit(builder, stack_size);
  if (result) {
    return result;
  }

  // Copy the final code to the cache and relocate it.
  out_code_size = getSize();
  out_code_address = Emplace(stack_size);

  // Stash source map.
  if (debug_info_flags & DEBUG_INFO_SOURCE_MAP) {
    debug_info->InitializeSourceMap(
        source_map_count_,
        (SourceMapEntry*)source_map_arena_.CloneContents());
  }

  return 0;
}

void* X64Emitter::Emplace(size_t stack_size) {
  // To avoid changing xbyak, we do a switcharoo here.
  // top_ points to the Xbyak buffer, and since we are in AutoGrow mode
  // it has pending relocations. We copy the top_ to our buffer, swap the
  // pointer, relocate, then return the original scratch pointer for use.
  uint8_t* old_address = top_;
  void* new_address = code_cache_->PlaceCode(top_, size_, stack_size);
  top_ = (uint8_t*)new_address;
  ready();
  top_ = old_address;
  reset();
  return new_address;
}

int X64Emitter::Emit(HIRBuilder* builder, size_t& out_stack_size) {
  // Calculate stack size. We need to align things to their natural sizes.
  // This could be much better (sort by type/etc).
  auto locals = builder->locals();
  size_t stack_offset = StackLayout::GUEST_STACK_SIZE;
  for (auto it = locals.begin(); it != locals.end(); ++it) {
    auto slot = *it;
    size_t type_size = GetTypeSize(slot->type);
    // Align to natural size.
    stack_offset = XEALIGN(stack_offset, type_size);
    slot->set_constant((uint32_t)stack_offset);
    stack_offset += type_size;
  }
  // Ensure 16b alignment.
  stack_offset -= StackLayout::GUEST_STACK_SIZE;
  stack_offset = XEALIGN(stack_offset, 16);

  // Function prolog.
  // Must be 16b aligned.
  // Windows is very strict about the form of this and the epilog:
  // http://msdn.microsoft.com/en-us/library/tawsa7cb.aspx
  // TODO(benvanik): save off non-volatile registers so we can use them:
  //     RBX, RBP, RDI, RSI, RSP, R12, R13, R14, R15
  //     Only want to do this if we actually use them, though, otherwise
  //     it just adds overhead.
  // IMPORTANT: any changes to the prolog must be kept in sync with
  //     X64CodeCache, which dynamically generates exception information.
  //     Adding or changing anything here must be matched!
  const bool emit_prolog = true;
  const size_t stack_size = StackLayout::GUEST_STACK_SIZE + stack_offset;
  XEASSERT((stack_size + 8) % 16 == 0);
  out_stack_size = stack_size;
  stack_size_ = stack_size;
  if (emit_prolog) {
    sub(rsp, (uint32_t)stack_size);
    mov(qword[rsp + StackLayout::GUEST_RCX_HOME], rcx);
    mov(qword[rsp + StackLayout::GUEST_RET_ADDR], rdx);
    mov(qword[rsp + StackLayout::GUEST_CALL_RET_ADDR], 0);
    mov(rdx, qword[rcx + 8]); // membase
  }

  // Body.
  auto block = builder->first_block();
  while (block) {
    // Mark block labels.
    auto label = block->label_head;
    while (label) {
      L(label->name);
      label = label->next;
    }

    // Process instructions.
    const Instr* instr = block->instr_head;
    while (instr) {
      const Instr* new_tail = instr;
      if (!SelectSequence(*this, instr, &new_tail)) {
        // No sequence found!
        XEASSERTALWAYS();
        XELOGE("Unable to process HIR opcode %s", instr->opcode->name);
        break;
      }
      instr = new_tail;
    }

    block = block->next;
  }

  // Function epilog.
  L("epilog");
  if (emit_prolog) {
    mov(rcx, qword[rsp + StackLayout::GUEST_RCX_HOME]);
    add(rsp, (uint32_t)stack_size);
  }
  ret();

#if XE_DEBUG
  nop();
  nop();
  nop();
  nop();
  nop();
#endif  // XE_DEBUG

  return 0;
}

void X64Emitter::MarkSourceOffset(const Instr* i) {
  auto entry = source_map_arena_.Alloc<SourceMapEntry>();
  entry->source_offset  = i->src1.offset;
  entry->hir_offset     = uint32_t(i->block->ordinal << 16) | i->ordinal;
  entry->code_offset    = getSize();
  source_map_count_++;
}

void X64Emitter::DebugBreak() {

}

void X64Emitter::Trap() {

}

void X64Emitter::UnimplementedInstr(const hir::Instr* i) {

}

uint64_t ResolveFunctionSymbol(void* raw_context, uint64_t symbol_info_ptr) {
  // TODO(benvanik): generate this thunk at runtime? or a shim?
  auto thread_state = *reinterpret_cast<ThreadState**>(raw_context);
  auto symbol_info = reinterpret_cast<FunctionInfo*>(symbol_info_ptr);

  Function* fn = NULL;
  thread_state->runtime()->ResolveFunction(symbol_info->address(), &fn);
  XEASSERTNOTNULL(fn);
  auto x64_fn = static_cast<X64Function*>(fn);
  return reinterpret_cast<uint64_t>(x64_fn->machine_code());
}
void X64Emitter::Call(const hir::Instr* instr, runtime::FunctionInfo* symbol_info) {
  auto fn = reinterpret_cast<X64Function*>(symbol_info->function());
  // Resolve address to the function to call and store in rax.
  // TODO(benvanik): caching/etc. For now this makes debugging easier.
  if (fn) {
    mov(rax, reinterpret_cast<uint64_t>(fn->machine_code()));
  } else {
    CallNative(ResolveFunctionSymbol, reinterpret_cast<uint64_t>(symbol_info));
  }

  // Actually jump/call to rax.
  if (instr->flags & CALL_TAIL) {
    // Pass the callers return address over.
    mov(rdx, qword[rsp + StackLayout::GUEST_RET_ADDR]);

    add(rsp, static_cast<uint32_t>(stack_size()));
    jmp(rax);
  } else {
    // Return address is from the previous SET_RETURN_ADDRESS.
    mov(rdx, qword[rsp + StackLayout::GUEST_CALL_RET_ADDR]);
    call(rax);
  }
}

uint64_t ResolveFunctionAddress(void* raw_context, uint64_t target_address) {
  // TODO(benvanik): generate this thunk at runtime? or a shim?
  auto thread_state = *reinterpret_cast<ThreadState**>(raw_context);

  // TODO(benvanik): required?
  target_address &= 0xFFFFFFFF;

  Function* fn = NULL;
  thread_state->runtime()->ResolveFunction(target_address, &fn);
  XEASSERTNOTNULL(fn);
  auto x64_fn = static_cast<X64Function*>(fn);
  return reinterpret_cast<uint64_t>(x64_fn->machine_code());
}
void X64Emitter::CallIndirect(const hir::Instr* instr, const Reg64& reg) {
  // Check if return.
  if (instr->flags & CALL_POSSIBLE_RETURN) {
    cmp(reg.cvt32(), dword[rsp + StackLayout::GUEST_RET_ADDR]);
    je("epilog", CodeGenerator::T_NEAR);
  }

  // Resolve address to the function to call and store in rax.
  // TODO(benvanik): caching/etc. For now this makes debugging easier.
  if (reg.getIdx() != rdx.getIdx()) {
    mov(rdx, reg);
  }
  CallNative(ResolveFunctionAddress);

  // Actually jump/call to rax.
  if (instr->flags & CALL_TAIL) {
    // Pass the callers return address over.
    mov(rdx, qword[rsp + StackLayout::GUEST_RET_ADDR]);

    add(rsp, static_cast<uint32_t>(stack_size()));
    jmp(rax);
  } else {
    // Return address is from the previous SET_RETURN_ADDRESS.
    mov(rdx, qword[rsp + StackLayout::GUEST_CALL_RET_ADDR]);
    call(rax);
  }
}

uint64_t UndefinedCallExtern(void* raw_context, uint64_t symbol_info_ptr) {
  auto symbol_info = reinterpret_cast<FunctionInfo*>(symbol_info_ptr);
  XELOGW("undefined extern call to %.8X %s",
         symbol_info->address(),
         symbol_info->name());
  return 0;
}
void X64Emitter::CallExtern(const hir::Instr* instr, const FunctionInfo* symbol_info) {
  XEASSERT(symbol_info->behavior() == FunctionInfo::BEHAVIOR_EXTERN);
  if (!symbol_info->extern_handler()) {
    CallNative(UndefinedCallExtern, reinterpret_cast<uint64_t>(symbol_info));
  } else {
    // rcx = context
    // rdx = target host function
    // r8  = arg0
    // r9  = arg1
    mov(rdx, reinterpret_cast<uint64_t>(symbol_info->extern_handler()));
    mov(r8, reinterpret_cast<uint64_t>(symbol_info->extern_arg0()));
    mov(r9, reinterpret_cast<uint64_t>(symbol_info->extern_arg1()));
    auto thunk = backend()->guest_to_host_thunk();
    mov(rax, reinterpret_cast<uint64_t>(thunk));
    call(rax);
    mov(rcx, qword[rsp + StackLayout::GUEST_RCX_HOME]);
    mov(rdx, qword[rcx + 8]); // membase
    // rax = host return
  }
}

void X64Emitter::CallNative(void* fn) {
  mov(rax, reinterpret_cast<uint64_t>(fn));
  call(rax);
  mov(rcx, qword[rsp + StackLayout::GUEST_RCX_HOME]);
  mov(rdx, qword[rcx + 8]); // membase
}

void X64Emitter::CallNative(uint64_t(*fn)(void* raw_context)) {
  mov(rax, reinterpret_cast<uint64_t>(fn));
  call(rax);
  mov(rcx, qword[rsp + StackLayout::GUEST_RCX_HOME]);
  mov(rdx, qword[rcx + 8]); // membase
}

void X64Emitter::CallNative(uint64_t(*fn)(void* raw_context, uint64_t arg0)) {
  mov(rax, reinterpret_cast<uint64_t>(fn));
  call(rax);
  mov(rcx, qword[rsp + StackLayout::GUEST_RCX_HOME]);
  mov(rdx, qword[rcx + 8]); // membase
}

void X64Emitter::CallNative(uint64_t(*fn)(void* raw_context, uint64_t arg0), uint64_t arg0) {
  mov(rdx, arg0);
  mov(rax, reinterpret_cast<uint64_t>(fn));
  call(rax);
  mov(rcx, qword[rsp + StackLayout::GUEST_RCX_HOME]);
  mov(rdx, qword[rcx + 8]); // membase
}

void X64Emitter::SetReturnAddress(uint64_t value) {
  mov(qword[rsp + StackLayout::GUEST_CALL_RET_ADDR], value);
}

void X64Emitter::LoadEflags() {
#if STORE_EFLAGS
  mov(eax, dword[rsp + STASH_OFFSET]);
  push(rax);
  popf();
#else
  // EFLAGS already present.
#endif  // STORE_EFLAGS
}

void X64Emitter::StoreEflags() {
#if STORE_EFLAGS
  pushf();
  pop(qword[rsp + STASH_OFFSET]);
#else
  // EFLAGS should have CA set?
  // (so long as we don't fuck with it)
#endif  // STORE_EFLAGS
}

Address X64Emitter::GetXmmPtr(XmmConst id) {
  static const vec128_t xmm_consts[] = {
    /* XMMZero                */ vec128f(0.0f, 0.0f, 0.0f, 0.0f),
    /* XMMOne                 */ vec128f(1.0f, 1.0f, 1.0f, 1.0f),
    /* XMMNegativeOne         */ vec128f(-1.0f, -1.0f, -1.0f, -1.0f),
    /* XMMMaskX16Y16          */ vec128i(0x0000FFFF, 0xFFFF0000, 0x00000000, 0x00000000),
    /* XMMFlipX16Y16          */ vec128i(0x00008000, 0x00000000, 0x00000000, 0x00000000),
    /* XMMFixX16Y16           */ vec128f(-32768.0f, 0.0f, 0.0f, 0.0f),
    /* XMMNormalizeX16Y16     */ vec128f(1.0f / 32767.0f, 1.0f / (32767.0f * 65536.0f), 0.0f, 0.0f),
    /* XMM3301                */ vec128f(3.0f, 3.0f, 0.0f, 1.0f),
    /* XMMSignMaskPS          */ vec128i(0x80000000u, 0x80000000u, 0x80000000u, 0x80000000u),
    /* XMMSignMaskPD          */ vec128i(0x00000000u, 0x80000000u, 0x00000000u, 0x80000000u),
    /* XMMByteSwapMask        */ vec128i(0x00010203u, 0x04050607u, 0x08090A0Bu, 0x0C0D0E0Fu),
    /* XMMPermuteControl15    */ vec128b(15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15),
    /* XMMUnpackD3DCOLOR      */ vec128i(0xFFFFFF02, 0xFFFFFF01, 0xFFFFFF00, 0xFFFFFF02),
    /* XMMOneOver255          */ vec128f(1.0f / 255.0f, 1.0f / 255.0f, 1.0f / 255.0f, 1.0f / 255.0f),
    /* XMMShiftMaskPS         */ vec128f(0x0000001Fu, 0x0000001Fu, 0x0000001Fu, 0x0000001Fu),
  };
  mov(rax, (uint64_t)&xmm_consts[id]);
  return ptr[rax];
}
