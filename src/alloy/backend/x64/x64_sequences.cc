/**
 ******************************************************************************
 * Xenia : Xbox 360 Emulator Research Project                                 *
 ******************************************************************************
 * Copyright 2014 Ben Vanik. All rights reserved.                             *
 * Released under the BSD license - see LICENSE in the root for more details. *
 ******************************************************************************
 */

// A note about vectors:
// Alloy represents vectors as xyzw pairs, with indices 0123.
// XMM registers are xyzw pairs with indices 3210, making them more like wzyx.
// This makes things somewhat confusing. It'd be nice to just shuffle the
// registers around on load/store, however certain operations require that
// data be in the right offset.
// Basically, this identity must hold:
//   shuffle(vec, b00011011) -> {x,y,z,w} => {x,y,z,w}
// All indices and operations must respect that.
//
// Memory (big endian):
// [00 01 02 03] [04 05 06 07] [08 09 0A 0B] [0C 0D 0E 0F] (x, y, z, w)
// load into xmm register:
// [0F 0E 0D 0C] [0B 0A 09 08] [07 06 05 04] [03 02 01 00] (w, z, y, x)

#include <alloy/backend/x64/x64_sequences.h>

#include <alloy/backend/x64/x64_emitter.h>
#include <alloy/backend/x64/x64_tracers.h>
#include <alloy/hir/hir_builder.h>

using namespace alloy;
using namespace alloy::backend;
using namespace alloy::backend::x64;
using namespace alloy::hir;
using namespace alloy::runtime;

using namespace Xbyak;

//
//class ValueI32 {};
//class ValueI64 {};
//class ConstI32 {};
//class ConstI64 {};
//
//template <hir::Opcode OPCODE, typename IMPL>
//class Instruction {
//  //void Emit(const Instr& i, D& d, A& a, B& b) {}
//  void Emit(const Instr& i) {
//    const OpcodeInfo* info = i.opcode;
//
//    OpcodeSignatureType dest_type = GET_OPCODE_SIG_TYPE_DEST(info->signature);
//    OpcodeSignatureType src1_type = GET_OPCODE_SIG_TYPE_SRC1(info->signature);
//    OpcodeSignatureType src2_type = GET_OPCODE_SIG_TYPE_SRC2(info->signature);
//    OpcodeSignatureType src3_type = GET_OPCODE_SIG_TYPE_SRC3(info->signature);
//  /*OPCODE_SIG_X        = (OPCODE_SIG_TYPE_X),
//  OPCODE_SIG_X_L      = (OPCODE_SIG_TYPE_X) | (OPCODE_SIG_TYPE_L << 3),
//  OPCODE_SIG_X_O      = (OPCODE_SIG_TYPE_X) | (OPCODE_SIG_TYPE_O << 3),
//  OPCODE_SIG_X_O_V    = (OPCODE_SIG_TYPE_X) | (OPCODE_SIG_TYPE_O << 3) | (OPCODE_SIG_TYPE_V << 6),
//  OPCODE_SIG_X_S      = (OPCODE_SIG_TYPE_X) | (OPCODE_SIG_TYPE_S << 3),
//  OPCODE_SIG_X_V      = (OPCODE_SIG_TYPE_X) | (OPCODE_SIG_TYPE_V << 3),
//  OPCODE_SIG_X_V_L    = (OPCODE_SIG_TYPE_X) | (OPCODE_SIG_TYPE_V << 3) | (OPCODE_SIG_TYPE_L << 6),
//  OPCODE_SIG_X_V_L_L  = (OPCODE_SIG_TYPE_X) | (OPCODE_SIG_TYPE_V << 3) | (OPCODE_SIG_TYPE_L << 6) | (OPCODE_SIG_TYPE_L << 9),
//  OPCODE_SIG_X_V_O    = (OPCODE_SIG_TYPE_X) | (OPCODE_SIG_TYPE_V << 3) | (OPCODE_SIG_TYPE_O << 6),
//  OPCODE_SIG_X_V_S    = (OPCODE_SIG_TYPE_X) | (OPCODE_SIG_TYPE_V << 3) | (OPCODE_SIG_TYPE_S << 6),
//  OPCODE_SIG_X_V_V    = (OPCODE_SIG_TYPE_X) | (OPCODE_SIG_TYPE_V << 3) | (OPCODE_SIG_TYPE_V << 6),
//  OPCODE_SIG_X_V_V_V  = (OPCODE_SIG_TYPE_X) | (OPCODE_SIG_TYPE_V << 3) | (OPCODE_SIG_TYPE_V << 6) | (OPCODE_SIG_TYPE_V << 9),
//  OPCODE_SIG_V        = (OPCODE_SIG_TYPE_V),
//  OPCODE_SIG_V_O      = (OPCODE_SIG_TYPE_V) | (OPCODE_SIG_TYPE_O << 3),
//  OPCODE_SIG_V_V      = (OPCODE_SIG_TYPE_V) | (OPCODE_SIG_TYPE_V << 3),
//  OPCODE_SIG_V_V_O    = (OPCODE_SIG_TYPE_V) | (OPCODE_SIG_TYPE_V << 3) | (OPCODE_SIG_TYPE_O << 6),
//  OPCODE_SIG_V_V_O_V  = (OPCODE_SIG_TYPE_V) | (OPCODE_SIG_TYPE_V << 3) | (OPCODE_SIG_TYPE_O << 6) | (OPCODE_SIG_TYPE_V << 9),
//  OPCODE_SIG_V_V_V    = (OPCODE_SIG_TYPE_V) | (OPCODE_SIG_TYPE_V << 3) | (OPCODE_SIG_TYPE_V << 6),
//  OPCODE_SIG_V_V_V_O  = (OPCODE_SIG_TYPE_V) | (OPCODE_SIG_TYPE_V << 3) | (OPCODE_SIG_TYPE_V << 6) | (OPCODE_SIG_TYPE_O << 9),
//  OPCODE_SIG_V_V_V_V  = (OPCODE_SIG_TYPE_V) | (OPCODE_SIG_TYPE_V << 3) | (OPCODE_SIG_TYPE_V << 6) | (OPCODE_SIG_TYPE_V << 9),*/
//    //
//  }
//};

template <typename... Ts>
struct CombinedStruct;
template <>
struct CombinedStruct<> {};
template <typename T, typename... Ts>
struct CombinedStruct<T, Ts...> : T, CombinedStruct<Ts...> {};

template <typename T, OpcodeSignatureType SIG_TYPE>
struct Op {
  static const OpcodeSignatureType sig_type = SIG_TYPE;

  template<typename T, OpcodeSignatureType SIG_TYPE_T>
  class Type;
  template<> struct Type<T, OPCODE_SIG_TYPE_O> {
    typedef uint64_t type;
    static bool Load(T* t, const Instr::Op* op) {
      return t->Load(op->offset);
    }
  };
  template<> struct Type<T, OPCODE_SIG_TYPE_S> {
    typedef FunctionInfo* type;
    static bool Load(T* t, const Instr::Op* op) {
      return t->Load(op->symbol_info);
    }
  };
  template<> struct Type<T, OPCODE_SIG_TYPE_L> {
    typedef hir::Label* type;
    static bool Load(T* t, const Instr::Op* op) {
      return t->Load(op->label);
    }
  };
  template<> struct Type<T, OPCODE_SIG_TYPE_V> {
    typedef Value* type;
    static bool Load(T* t, const Instr::Op* op) {
      return t->Load(op->value);
    }
  };
  typedef typename Type<T, SIG_TYPE>::type type;

protected:
  template <typename DEST, typename... Tf> friend struct InstrFields;
  bool LoadOp(const Instr::Op* op) {
    return Type<T, SIG_TYPE>::Load((T*)this, op);
  }
};

struct O64 : Op<O64, OPCODE_SIG_TYPE_O> {
  uint64_t value;

protected:
  template <typename T, OpcodeSignatureType SIG_TYPE> friend struct Op;
  template <typename DEST, typename... Tf> friend struct InstrFields;
  bool Load(uint64_t offset) {
    this->value = offset;
    return true;
  }
};

struct SYM : Op<SYM, OPCODE_SIG_TYPE_S> {
  FunctionInfo* value;

protected:
  template <typename T, OpcodeSignatureType SIG_TYPE> friend struct Op;
  template <typename DEST, typename... Tf> friend struct InstrFields;
  bool Load(FunctionInfo* value) {
    this->value = value;
    return true;
  }
};

struct LBL : Op<LBL, OPCODE_SIG_TYPE_L> {
  hir::Label* value;

protected:
  template <typename T, OpcodeSignatureType SIG_TYPE> friend struct Op;
  template <typename DEST, typename... Tf> friend struct InstrFields;
  bool Load(hir::Label* value) {
    this->value = value;
    return true;
  }
};

template <typename T, int TAG = -1>
struct ValueOp : Op<ValueOp<T, TAG>, OPCODE_SIG_TYPE_V> {
  static const int tag = TAG;
  const Value* value;

protected:
  template <typename T, OpcodeSignatureType SIG_TYPE> friend struct Op;
  template <typename DEST, typename... Tf> friend struct InstrFields;
  virtual bool Load(const Value* value) {
    this->value = value;
    return true;
  }
};

template <int TAG = -1>
struct I8 : ValueOp<I8<TAG>, TAG> {
  Reg8 reg;
protected:
  template <typename T, OpcodeSignatureType SIG_TYPE> friend struct Op;
  template <typename DEST, typename... Tf> friend struct InstrFields;
  bool Load(const Value* value) override {
    if (value->type == INT8_TYPE &&
        ValueOp<I8<TAG>, TAG>::Load(value)) {
      X64Emitter::SetupReg(value, reg);
      return true;
    }
    return false;
  }
};
template <int TAG = -1>
struct I16 : ValueOp<I16<TAG>, TAG> {
  Reg16 reg;
protected:
  template <typename T, OpcodeSignatureType SIG_TYPE> friend struct Op;
  template <typename DEST, typename... Tf> friend struct InstrFields;
  bool Load(const Value* value) override {
    if (value->type == INT16_TYPE &&
        ValueOp<I16<TAG>, TAG>::Load(value)) {
      X64Emitter::SetupReg(value, reg);
      return true;
    }
    return false;
  }
};
template <int TAG = -1>
struct I32 : ValueOp<I32<TAG>, TAG> {
  Reg32 reg;
protected:
  template <typename T, OpcodeSignatureType SIG_TYPE> friend struct Op;
  template <typename DEST, typename... Tf> friend struct InstrFields;
  bool Load(const Value* value) override {
    if (value->type == INT32_TYPE &&
        ValueOp<I32<TAG>, TAG>::Load(value)) {
      X64Emitter::SetupReg(value, reg);
      return true;
    }
    return false;
  }
};
template <int TAG = -1>
struct I64 : ValueOp<I64<TAG>, TAG> {
  Reg64 reg;
protected:
  template <typename T, OpcodeSignatureType SIG_TYPE> friend struct Op;
  template <typename DEST, typename... Tf> friend struct InstrFields;
  bool Load(const Value* value) override {
    if (value->type == INT64_TYPE &&
        ValueOp<I64<TAG>, TAG>::Load(value)) {
      X64Emitter::SetupReg(value, reg);
      return true;
    }
    return false;
  }
};
template <int TAG = -1>
struct F32 : ValueOp<F32<TAG>, TAG> {
  Xmm reg;
protected:
  template <typename T, OpcodeSignatureType SIG_TYPE> friend struct Op;
  template <typename DEST, typename... Tf> friend struct InstrFields;
  bool Load(const Value* value) override {
    if (value->type == FLOAT32_TYPE &&
        ValueOp<F32<TAG>, TAG>::Load(value)) {
      X64Emitter::SetupReg(value, reg);
      return true;
    }
    return false;
  }
};
template <int TAG = -1>
struct F64 : ValueOp<F64<TAG>, TAG> {
  Xmm reg;
protected:
  template <typename T, OpcodeSignatureType SIG_TYPE> friend struct Op;
  template <typename DEST, typename... Tf> friend struct InstrFields;
  bool Load(const Value* value) override {
    if (value->type == FLOAT64_TYPE &&
        ValueOp<F64<TAG>, TAG>::Load(value)) {
      X64Emitter::SetupReg(value, reg);
      return true;
    }
    return false;
  }
};
template <int TAG = -1>
struct V128 : ValueOp<V128<TAG>, TAG> {
  Xmm reg;
protected:
  template <typename T, OpcodeSignatureType SIG_TYPE> friend struct Op;
  template <typename DEST, typename... Tf> friend struct InstrFields;
  bool Load(const Value* value) override {
    if (value->type == VEC128_TYPE &&
        ValueOp<V128<TAG>, TAG>::Load(value)) {
      X64Emitter::SetupReg(value, reg);
      return true;
    }
    return false;
  }
};
template <int TAG = -1>
struct I8C: ValueOp<I8C<TAG>, TAG> {
  int8_t constant;
protected:
  template <typename T, OpcodeSignatureType SIG_TYPE> friend struct Op;
  template <typename DEST, typename... Tf> friend struct InstrFields;
  bool Load(const Value* value) override {
    if (value->type == INT8_TYPE &&
        ValueOp<I8C<TAG>, TAG>::Load(value)) {
      constant = value->constant.i8;
      return true;
    }
    return false;
  }
};
template <int TAG = -1>
struct I16C : ValueOp<I16C<TAG>, TAG> {
  int16_t constant;
protected:
  template <typename T, OpcodeSignatureType SIG_TYPE> friend struct Op;
  template <typename DEST, typename... Tf> friend struct InstrFields;
  bool Load(const Value* value) override {
    if (value->type == INT16_TYPE &&
        ValueOp<I16C<TAG>, TAG>::Load(value)) {
      constant = value->constant.i16;
      return true;
    }
    return false;
  }
};
template <int TAG = -1>
struct I32C : ValueOp<I32C<TAG>, TAG> {
  int32_t constant;
protected:
  template <typename T, OpcodeSignatureType SIG_TYPE> friend struct Op;
  template <typename DEST, typename... Tf> friend struct InstrFields;
  bool Load(const Value* value) override {
    if (value->type == INT32_TYPE &&
        ValueOp<I32C<TAG>, TAG>::Load(value)) {
      constant = value->constant.i32;
      return true;
    }
    return false;
  }
};
template <int TAG = -1>
struct I64C : ValueOp<I64C<TAG>, TAG> {
  int64_t constant;
protected:
  template <typename T, OpcodeSignatureType SIG_TYPE> friend struct Op;
  template <typename DEST, typename... Tf> friend struct InstrFields;
  bool Load(const Value* value) override {
    if (value->type == INT64_TYPE &&
        ValueOp<I64C<TAG>, TAG>::Load(value)) {
      constant = value->constant.i64;
      return true;
    }
    return false;
  }
};
template <int TAG = -1>
struct F32C : ValueOp<F32C<TAG>, TAG> {
  float constant;
protected:
  template <typename T, OpcodeSignatureType SIG_TYPE> friend struct Op;
  template <typename DEST, typename... Tf> friend struct InstrFields;
  bool Load(const Value* value) override {
    if (value->type == FLOAT32_TYPE &&
        ValueOp<F32C<TAG>, TAG>::Load(value)) {
      constant = value->constant.f32;
      return true;
    }
    return false;
  }
};
template <int TAG = -1>
struct F64C : ValueOp<F64C<TAG>, TAG> {
  double constant;
protected:
  template <typename T, OpcodeSignatureType SIG_TYPE> friend struct Op;
  template <typename DEST, typename... Tf> friend struct InstrFields;
  bool Load(const Value* value) override {
    if (value->type == FLOAT64_TYPE &&
        ValueOp<F64C<TAG>, TAG>::Load(value)) {
      constant = value->constant.f64;
      return true;
    }
    return false;
  }
};


struct TagTable {
  struct {
    bool valid;
    Instr::Op op;
  } table[16];
  template <typename T>
  typename std::enable_if<T::sig_type != OPCODE_SIG_TYPE_V, bool>::type
  CheckTag(const Instr::Op* op) {
    return true;
  }
  template <typename T>
  typename std::enable_if<T::sig_type == OPCODE_SIG_TYPE_V, bool>::type
  CheckTag(const Instr::Op* op) {
    return CheckTag<T>(op->value);
  }
  template <typename T>
  bool CheckTag(const Value* value) {
    if (T::tag == -1) {
      return true;
    }
    if (table[T::tag].valid &&
        table[T::tag].op.value != value) {
      return false;
    }
    table[T::tag].valid = true;
    table[T::tag].op.value = (Value*)value;
    return true;
  }
};


template <typename DEST, typename... Tf>
struct InstrFields;
template <typename DEST>
struct InstrFields<DEST> {
  DEST dest;

protected:
  bool LoadFields(const Instr* i, TagTable& tag_table) {
    return
          tag_table.CheckTag<DEST>(i->dest) &&
          dest.Load(i->dest);
  }
};
template <>
struct InstrFields<void> {
protected:
  bool LoadFields(const Instr* i, TagTable& tag_table) {
    return true;
  }
};
template <typename DEST, typename SRC1>
struct InstrFields<DEST, SRC1> : InstrFields<DEST> {
  SRC1 src1;

protected:
  bool LoadFields(const Instr* i, TagTable& tag_table) {
    return
          InstrFields<DEST>::LoadFields(i, tag_table) &&
          tag_table.CheckTag<SRC1>(&i->src1) &&
          src1.LoadOp(&i->src1);
  }
};
template <typename DEST, typename SRC1, typename SRC2>
struct InstrFields<DEST, SRC1, SRC2> : InstrFields<DEST, SRC1> {
  SRC2 src2;

protected:
  bool LoadFields(const Instr* i, TagTable& tag_table) {
    return
          InstrFields<DEST, SRC1>::LoadFields(i, tag_table) &&
          tag_table.CheckTag<SRC2>(&i->src2) &&
          src2.LoadOp(&i->src2);
  }
};
template <typename DEST, typename SRC1, typename SRC2, typename SRC3>
struct InstrFields<DEST, SRC1, SRC2, SRC3> : InstrFields<DEST, SRC1, SRC2> {
  SRC3 src3;

protected:
  bool LoadFields(const Instr* i, TagTable& tag_table) {
    return
          InstrFields<DEST, SRC1, SRC2>::LoadFields(i, tag_table) &&
          tag_table.CheckTag<SRC3>(&i->src3) &&
          src3.LoadOp(&i->src3);
  }
};

template <hir::Opcode OPCODE, typename DEST, typename... As>
struct I : InstrFields<DEST, As...> {
  const static hir::Opcode opcode = OPCODE;
  const Instr* instr;

protected:
  template <typename... Ti> friend struct SequenceFields;
  bool Load(const Instr* i, TagTable& tag_table) {
    if (i->opcode->num != OPCODE || !LoadFields(i, tag_table)) {
      return false;
    }
    instr = i;
    return true;
  }
};

template <typename... Ti>
struct SequenceFields;
template <typename I1>
struct SequenceFields<I1> {
  I1 i1;

protected:
  template <typename SEQ, typename... Ti> friend struct Sequence;
  bool Check(const Instr* i, TagTable& tag_table, const Instr** new_tail) {
    if (i1.Load(i, tag_table)) {
      *new_tail = i->next;
      return true;
    }
    return false;
  }
};
template <typename I1, typename I2>
struct SequenceFields<I1, I2> : SequenceFields<I1> {
  I2 i2;

protected:
  template <typename SEQ, typename... Ti> friend struct Sequence;
  bool Check(const Instr* i, TagTable& tag_table, const Instr** new_tail) {
    if (SequenceFields<I1>::Check(i, tag_table, new_tail)) {
      auto ni = i->next;
      if (ni && i2.Load(ni, tag_table)) {
        *new_tail = ni;
        return i;
      }
    }
    return false;
  }
};
template <typename I1, typename I2, typename I3>
struct SequenceFields<I1, I2, I3> : SequenceFields<I1, I2> {
  I3 i3;

protected:
  template <typename SEQ, typename... Ti> friend struct Sequence;
  bool Check(const Instr* i, TagTable& tag_table, const Instr** new_tail) {
    if (SequenceFields<I1, I2>::Check(i, tag_table, new_tail)) {
      auto ni = i->next;
      if (ni && i3.Load(ni, tag_table)) {
        *new_tail = ni;
        return i;
      }
    }
    return false;
  }
};
template <typename I1, typename I2, typename I3, typename I4>
struct SequenceFields<I1, I2, I3, I4> : SequenceFields<I1, I2, I3> {
  I4 i4;

protected:
  template <typename SEQ, typename... Ti> friend struct Sequence;
  bool Check(const Instr* i, TagTable& tag_table, const Instr** new_tail) {
    if (SequenceFields<I1, I2, I3>::Check(i, tag_table, new_tail)) {
      auto ni = i->next;
      if (ni && i4.Load(ni, tag_table)) {
        *new_tail = ni;
        return i;
      }
    }
    return false;
  }
};
template <typename I1, typename I2, typename I3, typename I4, typename I5>
struct SequenceFields<I1, I2, I3, I4, I5> : SequenceFields<I1, I2, I3, I4> {
  I5 i5;

protected:
  template <typename SEQ, typename... Ti> friend struct Sequence;
  bool Check(const Instr* i, TagTable& tag_table, const Instr** new_tail) {
    if (SequenceFields<I1, I2, I3, I4>::Check(i, tag_table, new_tail)) {
      auto ni = i->next;
      if (ni && i5.Load(ni, tag_table)) {
        *new_tail = ni;
        return i;
      }
    }
    return false;
  }
};

template <typename SEQ, typename... Ti>
struct Sequence {
  struct EmitArgs : SequenceFields<Ti...> {};

  static bool Select(X64Emitter& e, const Instr* i, const Instr** new_tail) {
    EmitArgs args;
    TagTable tag_table;
    if (!args.Check(i, tag_table, new_tail)) {
      return false;
    }
    SEQ::Emit(e, args);
    return true;
  }
};

template <typename SEQ, typename T>
struct SingleSequence : public Sequence<SingleSequence<SEQ, T>, T> {
  typedef T EmitArgType;
  static void Emit(X64Emitter& e, const EmitArgs& _) {
    SEQ::Emit(e, _.i1);
  }
};

static const int ANY = -1;
typedef int tag_t;
static const tag_t TAG0 = 0;
static const tag_t TAG1 = 1;
static const tag_t TAG2 = 2;
static const tag_t TAG3 = 3;
static const tag_t TAG4 = 4;
static const tag_t TAG5 = 5;
static const tag_t TAG6 = 6;
static const tag_t TAG7 = 7;
// hrmm, need match class? or specific? (AX, BX, etc)
typedef int reg_t;
static const reg_t REG_0 = 0;
static const reg_t REG_1 = 1;
static const reg_t REG_2 = 2;
static const reg_t REG_3 = 3;
static const reg_t REG_AX = (1 << 8) | 0;
static const reg_t REG_BX = (1 << 8) | 1;
static const reg_t REG_CX = (1 << 8) | 2;
static const reg_t REG_DX = (1 << 8) | 3;

//#define EMITTER(name, ...) struct name : SingleSequence<name, __VA_ARGS__>
#define MATCH(...) __VA_ARGS__
#define EMITTER(name, match) struct name : SingleSequence<name, match>
#define SEQUENCE(name, match) struct name : Sequence<name, match>

typedef bool (*SequenceSelectFn)(X64Emitter&, const Instr*, const Instr**);
#define EMITTER_OPCODE_TABLE(opcode, ...) \
  bool Select##opcode(X64Emitter& e, const Instr* i, const Instr** new_tail) { \
    static const SequenceSelectFn table[] = { __VA_ARGS__, nullptr }; \
    for (const SequenceSelectFn* p = &table[0]; p; ++p) { \
      if ((*p)(e, i, new_tail)) return true; \
    } \
    return false; \
  }


//SEQUENCE(ADD_ADD_BRANCH, MATCH(
//    I<OPCODE_ADD, I32<TAG0>, I32<>, I32C<>>,
//    I<OPCODE_ADD, I32<>, I32<TAG0>, I32C<>>,
//    I<OPCODE_BRANCH_TRUE, void, O64>)) {
//  static void Emit(X64Emitter& e, const EmitArgs& _) {
//  }
//};


// ============================================================================
// OPCODE_COMMENT
// ============================================================================
EMITTER(COMMENT, MATCH(I<OPCODE_COMMENT, void, O64>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    if (IsTracingInstr()) {
      auto str = reinterpret_cast<const char*>(i.src1.value);
      // TODO(benvanik): pass through.
      // TODO(benvanik): don't just leak this memory.
      auto str_copy = xestrdupa(str);
      e.mov(e.rdx, reinterpret_cast<uint64_t>(str_copy));
      e.CallNative(TraceString);
    }
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_COMMENT,
    COMMENT::Select);


// ============================================================================
// OPCODE_NOP
// ============================================================================
EMITTER(NOP, MATCH(I<OPCODE_NOP, void, O64>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.nop();
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_NOP,
    NOP::Select);


// ============================================================================
// OPCODE_SOURCE_OFFSET
// ============================================================================
EMITTER(SOURCE_OFFSET, MATCH(I<OPCODE_SOURCE_OFFSET, void, O64>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
#if XE_DEBUG
    e.nop();
    e.nop();
    e.mov(e.eax, (uint32_t)i.src1.value);
    e.nop();
    e.nop();
#endif  // XE_DEBUG
    e.MarkSourceOffset(i.instr);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_SOURCE_OFFSET,
    SOURCE_OFFSET::Select);


// ============================================================================
// OPCODE_DEBUG_BREAK
// ============================================================================
EMITTER(DEBUG_BREAK, MATCH(I<OPCODE_DEBUG_BREAK, void>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.DebugBreak();
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_DEBUG_BREAK,
    DEBUG_BREAK::Select);


// ============================================================================
// OPCODE_DEBUG_BREAK_TRUE
// ============================================================================
EMITTER(DEBUG_BREAK_TRUE_I8, MATCH(I<OPCODE_DEBUG_BREAK_TRUE, void, I8<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.test(i.src1.reg, i.src1.reg);
    Xbyak::Label skip;
    e.jz(skip);
    e.DebugBreak();
    e.L(skip);
  }
};
EMITTER(DEBUG_BREAK_TRUE_I16, MATCH(I<OPCODE_DEBUG_BREAK_TRUE, void, I16<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.test(i.src1.reg, i.src1.reg);
    Xbyak::Label skip;
    e.jz(skip);
    e.DebugBreak();
    e.L(skip);
  }
};
EMITTER(DEBUG_BREAK_TRUE_I32, MATCH(I<OPCODE_DEBUG_BREAK_TRUE, void, I32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.test(i.src1.reg, i.src1.reg);
    Xbyak::Label skip;
    e.jz(skip);
    e.DebugBreak();
    e.L(skip);
  }
};
EMITTER(DEBUG_BREAK_TRUE_I64, MATCH(I<OPCODE_DEBUG_BREAK_TRUE, void, I64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.test(i.src1.reg, i.src1.reg);
    Xbyak::Label skip;
    e.jz(skip);
    e.DebugBreak();
    e.L(skip);
  }
};
EMITTER(DEBUG_BREAK_TRUE_F32, MATCH(I<OPCODE_DEBUG_BREAK_TRUE, void, F32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vptest(i.src1.reg, i.src1.reg);
    Xbyak::Label skip;
    e.jz(skip);
    e.DebugBreak();
    e.L(skip);
  }
};
EMITTER(DEBUG_BREAK_TRUE_F64, MATCH(I<OPCODE_DEBUG_BREAK_TRUE, void, F64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vptest(i.src1.reg, i.src1.reg);
    Xbyak::Label skip;
    e.jz(skip);
    e.DebugBreak();
    e.L(skip);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_DEBUG_BREAK_TRUE,
    DEBUG_BREAK_TRUE_I8::Select,
    DEBUG_BREAK_TRUE_I16::Select,
    DEBUG_BREAK_TRUE_I32::Select,
    DEBUG_BREAK_TRUE_I64::Select,
    DEBUG_BREAK_TRUE_F32::Select,
    DEBUG_BREAK_TRUE_F64::Select);


// ============================================================================
// OPCODE_TRAP
// ============================================================================
EMITTER(TRAP, MATCH(I<OPCODE_TRAP, void, O64>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.Trap();
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_TRAP,
    TRAP::Select);


// ============================================================================
// OPCODE_TRAP_TRUE
// ============================================================================
EMITTER(TRAP_TRUE_I8, MATCH(I<OPCODE_TRAP_TRUE, void, I8<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.test(i.src1.reg, i.src1.reg);
    Xbyak::Label skip;
    e.jz(skip);
    e.Trap();
    e.L(skip);
  }
};
EMITTER(TRAP_TRUE_I16, MATCH(I<OPCODE_TRAP_TRUE, void, I16<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.test(i.src1.reg, i.src1.reg);
    Xbyak::Label skip;
    e.jz(skip);
    e.Trap();
    e.L(skip);
  }
};
EMITTER(TRAP_TRUE_I32, MATCH(I<OPCODE_TRAP_TRUE, void, I32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.test(i.src1.reg, i.src1.reg);
    Xbyak::Label skip;
    e.jz(skip);
    e.Trap();
    e.L(skip);
  }
};
EMITTER(TRAP_TRUE_I64, MATCH(I<OPCODE_TRAP_TRUE, void, I64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.test(i.src1.reg, i.src1.reg);
    Xbyak::Label skip;
    e.jz(skip);
    e.Trap();
    e.L(skip);
  }
};
EMITTER(TRAP_TRUE_F32, MATCH(I<OPCODE_TRAP_TRUE, void, F32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vptest(i.src1.reg, i.src1.reg);
    Xbyak::Label skip;
    e.jz(skip);
    e.Trap();
    e.L(skip);
  }
};
EMITTER(TRAP_TRUE_F64, MATCH(I<OPCODE_TRAP_TRUE, void, F64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vptest(i.src1.reg, i.src1.reg);
    Xbyak::Label skip;
    e.jz(skip);
    e.Trap();
    e.L(skip);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_TRAP_TRUE,
    TRAP_TRUE_I8::Select,
    TRAP_TRUE_I16::Select,
    TRAP_TRUE_I32::Select,
    TRAP_TRUE_I64::Select,
    TRAP_TRUE_F32::Select,
    TRAP_TRUE_F64::Select);


// ============================================================================
// OPCODE_CALL
// ============================================================================
EMITTER(CALL, MATCH(I<OPCODE_CALL, void, SYM>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.Call(i.instr, i.src1.value);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_CALL,
    CALL::Select);


// ============================================================================
// OPCODE_CALL_TRUE
// ============================================================================
EMITTER(CALL_TRUE_I8, MATCH(I<OPCODE_CALL_TRUE, void, I8<>, SYM>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.test(i.src1.reg, i.src1.reg);
    Xbyak::Label skip;
    e.jz(skip);
    e.Call(i.instr, i.src2.value);
    e.L(skip);
  }
};
EMITTER(CALL_TRUE_I16, MATCH(I<OPCODE_CALL_TRUE, void, I16<>, SYM>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.test(i.src1.reg, i.src1.reg);
    Xbyak::Label skip;
    e.jz(skip);
    e.Call(i.instr, i.src2.value);
    e.L(skip);
  }
};
EMITTER(CALL_TRUE_I32, MATCH(I<OPCODE_CALL_TRUE, void, I32<>, SYM>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.test(i.src1.reg, i.src1.reg);
    Xbyak::Label skip;
    e.jz(skip);
    e.Call(i.instr, i.src2.value);
    e.L(skip);
  }
};
EMITTER(CALL_TRUE_I64, MATCH(I<OPCODE_CALL_TRUE, void, I64<>, SYM>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.test(i.src1.reg, i.src1.reg);
    Xbyak::Label skip;
    e.jz(skip);
    e.Call(i.instr, i.src2.value);
    e.L(skip);
  }
};
EMITTER(CALL_TRUE_F32, MATCH(I<OPCODE_CALL_TRUE, void, F32<>, SYM>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vptest(i.src1.reg, i.src1.reg);
    Xbyak::Label skip;
    e.jz(skip);
    e.Call(i.instr, i.src2.value);
    e.L(skip);
  }
};
EMITTER(CALL_TRUE_F64, MATCH(I<OPCODE_CALL_TRUE, void, F64<>, SYM>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vptest(i.src1.reg, i.src1.reg);
    Xbyak::Label skip;
    e.jz(skip);
    e.Call(i.instr, i.src2.value);
    e.L(skip);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_CALL_TRUE,
    CALL_TRUE_I8::Select,
    CALL_TRUE_I16::Select,
    CALL_TRUE_I32::Select,
    CALL_TRUE_I64::Select,
    CALL_TRUE_F32::Select,
    CALL_TRUE_F64::Select);


// ============================================================================
// OPCODE_CALL_INDIRECT
// ============================================================================
EMITTER(CALL_INDIRECT, MATCH(I<OPCODE_CALL_INDIRECT, void, I64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.CallIndirect(i.instr, i.src1.reg);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_CALL_INDIRECT,
    CALL_INDIRECT::Select);


// ============================================================================
// OPCODE_CALL_INDIRECT_TRUE
// ============================================================================
EMITTER(CALL_INDIRECT_TRUE_I8, MATCH(I<OPCODE_CALL_INDIRECT_TRUE, void, I8<>, I64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.test(i.src1.reg, i.src1.reg);
    Xbyak::Label skip;
    e.jz(skip);
    e.CallIndirect(i.instr, i.src2.reg);
    e.L(skip);
  }
};
EMITTER(CALL_INDIRECT_TRUE_I16, MATCH(I<OPCODE_CALL_INDIRECT_TRUE, void, I16<>, I64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.test(i.src1.reg, i.src1.reg);
    Xbyak::Label skip;
    e.jz(skip);
    e.CallIndirect(i.instr, i.src2.reg);
    e.L(skip);
  }
};
EMITTER(CALL_INDIRECT_TRUE_I32, MATCH(I<OPCODE_CALL_INDIRECT_TRUE, void, I32<>, I64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.test(i.src1.reg, i.src1.reg);
    Xbyak::Label skip;
    e.jz(skip);
    e.CallIndirect(i.instr, i.src2.reg);
    e.L(skip);
  }
};
EMITTER(CALL_INDIRECT_TRUE_I64, MATCH(I<OPCODE_CALL_INDIRECT_TRUE, void, I64<>, I64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.test(i.src1.reg, i.src1.reg);
    Xbyak::Label skip;
    e.jz(skip);
    e.CallIndirect(i.instr, i.src2.reg);
    e.L(skip);
  }
};
EMITTER(CALL_INDIRECT_TRUE_F32, MATCH(I<OPCODE_CALL_INDIRECT_TRUE, void, F32<>, I64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vptest(i.src1.reg, i.src1.reg);
    Xbyak::Label skip;
    e.jz(skip);
    e.CallIndirect(i.instr, i.src2.reg);
    e.L(skip);
  }
};
EMITTER(CALL_INDIRECT_TRUE_F64, MATCH(I<OPCODE_CALL_INDIRECT_TRUE, void, F64<>, I64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vptest(i.src1.reg, i.src1.reg);
    Xbyak::Label skip;
    e.jz(skip);
    e.CallIndirect(i.instr, i.src2.reg);
    e.L(skip);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_CALL_INDIRECT_TRUE,
    CALL_INDIRECT_TRUE_I8::Select,
    CALL_INDIRECT_TRUE_I16::Select,
    CALL_INDIRECT_TRUE_I32::Select,
    CALL_INDIRECT_TRUE_I64::Select,
    CALL_INDIRECT_TRUE_F32::Select,
    CALL_INDIRECT_TRUE_F64::Select);


// ============================================================================
// OPCODE_CALL_EXTERN
// ============================================================================
EMITTER(CALL_EXTERN, MATCH(I<OPCODE_CALL_EXTERN, void, SYM>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.CallExtern(i.instr, i.src1.value);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_CALL_EXTERN,
    CALL_EXTERN::Select);


// ============================================================================
// OPCODE_RETURN
// ============================================================================
EMITTER(RETURN, MATCH(I<OPCODE_RETURN, void>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // If this is the last instruction in the last block, just let us
    // fall through.
    if (i.instr->next || i.instr->block->next) {
      e.jmp("epilog", CodeGenerator::T_NEAR);
    }
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_RETURN,
    RETURN::Select);


// ============================================================================
// OPCODE_RETURN_TRUE
// ============================================================================
EMITTER(RETURN_TRUE_I8, MATCH(I<OPCODE_RETURN_TRUE, void, I8<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.test(i.src1.reg, i.src1.reg);
    e.jnz("epilog", CodeGenerator::T_NEAR);
  }
};
EMITTER(RETURN_TRUE_I16, MATCH(I<OPCODE_RETURN_TRUE, void, I16<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.test(i.src1.reg, i.src1.reg);
    e.jnz("epilog", CodeGenerator::T_NEAR);
  }
};
EMITTER(RETURN_TRUE_I32, MATCH(I<OPCODE_RETURN_TRUE, void, I32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.test(i.src1.reg, i.src1.reg);
    e.jnz("epilog", CodeGenerator::T_NEAR);
  }
};
EMITTER(RETURN_TRUE_I64, MATCH(I<OPCODE_RETURN_TRUE, void, I64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.test(i.src1.reg, i.src1.reg);
    e.jnz("epilog", CodeGenerator::T_NEAR);
  }
};
EMITTER(RETURN_TRUE_F32, MATCH(I<OPCODE_RETURN_TRUE, void, F32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vptest(i.src1.reg, i.src1.reg);
    e.jnz("epilog", CodeGenerator::T_NEAR);
  }
};
EMITTER(RETURN_TRUE_F64, MATCH(I<OPCODE_RETURN_TRUE, void, F64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vptest(i.src1.reg, i.src1.reg);
    e.jnz("epilog", CodeGenerator::T_NEAR);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_RETURN_TRUE,
    RETURN_TRUE_I8::Select,
    RETURN_TRUE_I16::Select,
    RETURN_TRUE_I32::Select,
    RETURN_TRUE_I64::Select,
    RETURN_TRUE_F32::Select,
    RETURN_TRUE_F64::Select);


// ============================================================================
// OPCODE_SET_RETURN_ADDRESS
// ============================================================================
EMITTER(SET_RETURN_ADDRESS, MATCH(I<OPCODE_SET_RETURN_ADDRESS, void, I64C<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.SetReturnAddress(i.src1.constant);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_SET_RETURN_ADDRESS,
    SET_RETURN_ADDRESS::Select);


// ============================================================================
// OPCODE_BRANCH
// ============================================================================
EMITTER(BRANCH, MATCH(I<OPCODE_BRANCH, void, LBL>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.jmp(i.src1.value->name, e.T_NEAR);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_BRANCH,
    BRANCH::Select);


// ============================================================================
// OPCODE_BRANCH_TRUE
// ============================================================================
EMITTER(BRANCH_TRUE_I8, MATCH(I<OPCODE_BRANCH_TRUE, void, I8<>, LBL>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.test(i.src1.reg, i.src1.reg);
    e.jnz(i.src2.value->name, e.T_NEAR);
  }
};
EMITTER(BRANCH_TRUE_I16, MATCH(I<OPCODE_BRANCH_TRUE, void, I16<>, LBL>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.test(i.src1.reg, i.src1.reg);
    e.jnz(i.src2.value->name, e.T_NEAR);
  }
};
EMITTER(BRANCH_TRUE_I32, MATCH(I<OPCODE_BRANCH_TRUE, void, I32<>, LBL>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.test(i.src1.reg, i.src1.reg);
    e.jnz(i.src2.value->name, e.T_NEAR);
  }
};
EMITTER(BRANCH_TRUE_I64, MATCH(I<OPCODE_BRANCH_TRUE, void, I64<>, LBL>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.test(i.src1.reg, i.src1.reg);
    e.jnz(i.src2.value->name, e.T_NEAR);
  }
};
EMITTER(BRANCH_TRUE_F32, MATCH(I<OPCODE_BRANCH_TRUE, void, F32<>, LBL>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vptest(i.src1.reg, i.src1.reg);
    e.jnz(i.src2.value->name, e.T_NEAR);
  }
};
EMITTER(BRANCH_TRUE_F64, MATCH(I<OPCODE_BRANCH_TRUE, void, F64<>, LBL>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vptest(i.src1.reg, i.src1.reg);
    e.jnz(i.src2.value->name, e.T_NEAR);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_BRANCH_TRUE,
    BRANCH_TRUE_I8::Select,
    BRANCH_TRUE_I16::Select,
    BRANCH_TRUE_I32::Select,
    BRANCH_TRUE_I64::Select,
    BRANCH_TRUE_F32::Select,
    BRANCH_TRUE_F64::Select);


// ============================================================================
// OPCODE_BRANCH_FALSE
// ============================================================================
EMITTER(BRANCH_FALSE_I8, MATCH(I<OPCODE_BRANCH_FALSE, void, I8<>, LBL>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.test(i.src1.reg, i.src1.reg);
    e.jz(i.src2.value->name, e.T_NEAR);
  }
};
EMITTER(BRANCH_FALSE_I16, MATCH(I<OPCODE_BRANCH_FALSE, void, I16<>, LBL>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.test(i.src1.reg, i.src1.reg);
    e.jz(i.src2.value->name, e.T_NEAR);
  }
};
EMITTER(BRANCH_FALSE_I32, MATCH(I<OPCODE_BRANCH_FALSE, void, I32<>, LBL>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.test(i.src1.reg, i.src1.reg);
    e.jz(i.src2.value->name, e.T_NEAR);
  }
};
EMITTER(BRANCH_FALSE_I64, MATCH(I<OPCODE_BRANCH_FALSE, void, I64<>, LBL>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.test(i.src1.reg, i.src1.reg);
    e.jz(i.src2.value->name, e.T_NEAR);
  }
};
EMITTER(BRANCH_FALSE_F32, MATCH(I<OPCODE_BRANCH_FALSE, void, F32<>, LBL>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vptest(i.src1.reg, i.src1.reg);
    e.jz(i.src2.value->name, e.T_NEAR);
  }
};
EMITTER(BRANCH_FALSE_F64, MATCH(I<OPCODE_BRANCH_FALSE, void, F64<>, LBL>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vptest(i.src1.reg, i.src1.reg);
    e.jz(i.src2.value->name, e.T_NEAR);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_BRANCH_FALSE,
    BRANCH_FALSE_I8::Select,
    BRANCH_FALSE_I16::Select,
    BRANCH_FALSE_I32::Select,
    BRANCH_FALSE_I64::Select,
    BRANCH_FALSE_F32::Select,
    BRANCH_FALSE_F64::Select);


// ============================================================================
// OPCODE_ASSIGN
// ============================================================================
EMITTER(ASSIGN_I8, MATCH(I<OPCODE_ASSIGN, I8<>, I8<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.mov(i.dest.reg, i.src1.reg);
  }
};
EMITTER(ASSIGN_I16, MATCH(I<OPCODE_ASSIGN, I16<>, I16<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.mov(i.dest.reg, i.src1.reg);
  }
};
EMITTER(ASSIGN_I32, MATCH(I<OPCODE_ASSIGN, I32<>, I32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.mov(i.dest.reg, i.src1.reg);
  }
};
EMITTER(ASSIGN_I64, MATCH(I<OPCODE_ASSIGN, I64<>, I64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.mov(i.dest.reg, i.src1.reg);
  }
};
EMITTER(ASSIGN_F32, MATCH(I<OPCODE_ASSIGN, F32<>, F32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vmovaps(i.dest.reg, i.src1.reg);
  }
};
EMITTER(ASSIGN_F64, MATCH(I<OPCODE_ASSIGN, F64<>, F64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vmovaps(i.dest.reg, i.src1.reg);
  }
};
EMITTER(ASSIGN_V128, MATCH(I<OPCODE_ASSIGN, V128<>, V128<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vmovaps(i.dest.reg, i.src1.reg);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_ASSIGN,
    ASSIGN_I8::Select,
    ASSIGN_I16::Select,
    ASSIGN_I32::Select,
    ASSIGN_I64::Select,
    ASSIGN_F32::Select,
    ASSIGN_F64::Select,
    ASSIGN_V128::Select);


// ============================================================================
// OPCODE_CAST
// ============================================================================
EMITTER(CAST_I32_F32, MATCH(I<OPCODE_CAST, I32<>, F32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vmovd(i.dest.reg, i.src1.reg);
  }
};
EMITTER(CAST_I64_F64, MATCH(I<OPCODE_CAST, I64<>, F64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vmovq(i.dest.reg, i.src1.reg);
  }
};
EMITTER(CAST_F32_I32, MATCH(I<OPCODE_CAST, F32<>, I32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vmovd(i.dest.reg, i.src1.reg);
  }
};
EMITTER(CAST_F64_I64, MATCH(I<OPCODE_CAST, F64<>, I64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vmovq(i.dest.reg, i.src1.reg);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_CAST,
    CAST_I32_F32::Select,
    CAST_I64_F64::Select,
    CAST_F32_I32::Select,
    CAST_F64_I64::Select);


// ============================================================================
// OPCODE_ZERO_EXTEND
// ============================================================================
EMITTER(ZERO_EXTEND_I16_I8, MATCH(I<OPCODE_ZERO_EXTEND, I16<>, I8<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.movzx(i.dest.reg, i.src1.reg);
  }
};
EMITTER(ZERO_EXTEND_I32_I8, MATCH(I<OPCODE_ZERO_EXTEND, I32<>, I8<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.movzx(i.dest.reg, i.src1.reg);
  }
};
EMITTER(ZERO_EXTEND_I64_I8, MATCH(I<OPCODE_ZERO_EXTEND, I64<>, I8<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.movzx(i.dest.reg, i.src1.reg);
  }
};
EMITTER(ZERO_EXTEND_I32_I16, MATCH(I<OPCODE_ZERO_EXTEND, I32<>, I16<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.movzx(i.dest.reg, i.src1.reg);
  }
};
EMITTER(ZERO_EXTEND_I64_I16, MATCH(I<OPCODE_ZERO_EXTEND, I64<>, I16<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.movzx(i.dest.reg, i.src1.reg);
  }
};
EMITTER(ZERO_EXTEND_I64_I32, MATCH(I<OPCODE_ZERO_EXTEND, I64<>, I32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.mov(i.dest.reg.cvt32(), i.src1.reg);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_ZERO_EXTEND,
    ZERO_EXTEND_I16_I8::Select,
    ZERO_EXTEND_I32_I8::Select,
    ZERO_EXTEND_I64_I8::Select,
    ZERO_EXTEND_I32_I16::Select,
    ZERO_EXTEND_I64_I16::Select,
    ZERO_EXTEND_I64_I32::Select);


// ============================================================================
// OPCODE_SIGN_EXTEND
// ============================================================================
EMITTER(SIGN_EXTEND_I16_I8, MATCH(I<OPCODE_SIGN_EXTEND, I16<>, I8<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.movsx(i.dest.reg, i.src1.reg);
  }
};
EMITTER(SIGN_EXTEND_I32_I8, MATCH(I<OPCODE_SIGN_EXTEND, I32<>, I8<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.movsx(i.dest.reg, i.src1.reg);
  }
};
EMITTER(SIGN_EXTEND_I64_I8, MATCH(I<OPCODE_SIGN_EXTEND, I64<>, I8<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.movsx(i.dest.reg, i.src1.reg);
  }
};
EMITTER(SIGN_EXTEND_I32_I16, MATCH(I<OPCODE_SIGN_EXTEND, I32<>, I16<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.movsx(i.dest.reg, i.src1.reg);
  }
};
EMITTER(SIGN_EXTEND_I64_I16, MATCH(I<OPCODE_SIGN_EXTEND, I64<>, I16<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.movsx(i.dest.reg, i.src1.reg);
  }
};
EMITTER(SIGN_EXTEND_I64_I32, MATCH(I<OPCODE_SIGN_EXTEND, I64<>, I32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.movsxd(i.dest.reg, i.src1.reg);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_SIGN_EXTEND,
    SIGN_EXTEND_I16_I8::Select,
    SIGN_EXTEND_I32_I8::Select,
    SIGN_EXTEND_I64_I8::Select,
    SIGN_EXTEND_I32_I16::Select,
    SIGN_EXTEND_I64_I16::Select,
    SIGN_EXTEND_I64_I32::Select);


// ============================================================================
// OPCODE_TRUNCATE
// ============================================================================
EMITTER(TRUNCATE_I8_I16, MATCH(I<OPCODE_TRUNCATE, I8<>, I16<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.mov(i.dest.reg, i.src1.reg.cvt8());
  }
};
EMITTER(TRUNCATE_I8_I32, MATCH(I<OPCODE_TRUNCATE, I8<>, I32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.mov(i.dest.reg, i.src1.reg.cvt8());
  }
};
EMITTER(TRUNCATE_I8_I64, MATCH(I<OPCODE_TRUNCATE, I8<>, I64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.mov(i.dest.reg, i.src1.reg.cvt8());
  }
};
EMITTER(TRUNCATE_I16_I32, MATCH(I<OPCODE_TRUNCATE, I16<>, I32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.mov(i.dest.reg, i.src1.reg.cvt16());
  }
};
EMITTER(TRUNCATE_I16_I64, MATCH(I<OPCODE_TRUNCATE, I16<>, I64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.mov(i.dest.reg, i.src1.reg.cvt16());
  }
};
EMITTER(TRUNCATE_I32_I64, MATCH(I<OPCODE_TRUNCATE, I32<>, I64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.mov(i.dest.reg, i.src1.reg.cvt32());
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_TRUNCATE,
    TRUNCATE_I8_I16::Select,
    TRUNCATE_I8_I32::Select,
    TRUNCATE_I8_I64::Select,
    TRUNCATE_I16_I32::Select,
    TRUNCATE_I16_I64::Select,
    TRUNCATE_I32_I64::Select);


// ============================================================================
// OPCODE_CONVERT
// ============================================================================
EMITTER(CONVERT_I32_F32, MATCH(I<OPCODE_CONVERT, I32<>, F32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // TODO(benvanik): saturation check? cvtt* (trunc?)
    e.vcvtss2si(i.dest.reg, i.src1.reg);
  }
};
EMITTER(CONVERT_I32_F64, MATCH(I<OPCODE_CONVERT, I32<>, F64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // TODO(benvanik): saturation check? cvtt* (trunc?)
    e.vcvttss2si(i.dest.reg, i.src1.reg);
  }
};
EMITTER(CONVERT_I64_F64, MATCH(I<OPCODE_CONVERT, I64<>, F64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // TODO(benvanik): saturation check? cvtt* (trunc?)
    e.vcvttsd2si(i.dest.reg, i.src1.reg);
  }
};
EMITTER(CONVERT_F32_I32, MATCH(I<OPCODE_CONVERT, F32<>, I32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // TODO(benvanik): saturation check? cvtt* (trunc?)
    e.vcvtsi2ss(i.dest.reg, i.src1.reg);
  }
};
EMITTER(CONVERT_F32_F64, MATCH(I<OPCODE_CONVERT, F32<>, F64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // TODO(benvanik): saturation check? cvtt* (trunc?)
    e.vcvtsd2ss(i.dest.reg, i.src1.reg);
  }
};
EMITTER(CONVERT_F64_I64, MATCH(I<OPCODE_CONVERT, F64<>, I64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // TODO(benvanik): saturation check? cvtt* (trunc?)
    e.vcvtsi2sd(i.dest.reg, i.src1.reg);
  }
};
EMITTER(CONVERT_F64_F32, MATCH(I<OPCODE_CONVERT, F64<>, F32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vcvtss2sd(i.dest.reg, i.src1.reg);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_CONVERT,
    CONVERT_I32_F32::Select,
    CONVERT_I32_F64::Select,
    CONVERT_I64_F64::Select,
    CONVERT_F32_I32::Select,
    CONVERT_F32_F64::Select,
    CONVERT_F64_I64::Select,
    CONVERT_F64_F32::Select);


// ============================================================================
// OPCODE_ROUND
// ============================================================================
EMITTER(ROUND_F32, MATCH(I<OPCODE_ROUND, F32<>, F32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    switch (i.instr->flags) {
      case ROUND_TO_ZERO:
        e.vroundss(i.dest.reg, i.src1.reg, B00000011);
        break;
      case ROUND_TO_NEAREST:
        e.vroundss(i.dest.reg, i.src1.reg, B00000000);
        break;
      case ROUND_TO_MINUS_INFINITY:
        e.vroundss(i.dest.reg, i.src1.reg, B00000001);
        break;
      case ROUND_TO_POSITIVE_INFINITY:
        e.vroundss(i.dest.reg, i.src1.reg, B00000010);
        break;
    }
  }
};
EMITTER(ROUND_F64, MATCH(I<OPCODE_ROUND, F64<>, F64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    switch (i.instr->flags) {
      case ROUND_TO_ZERO:
        e.vroundsd(i.dest.reg, i.src1.reg, B00000011);
        break;
      case ROUND_TO_NEAREST:
        e.vroundsd(i.dest.reg, i.src1.reg, B00000000);
        break;
      case ROUND_TO_MINUS_INFINITY:
        e.vroundsd(i.dest.reg, i.src1.reg, B00000001);
        break;
      case ROUND_TO_POSITIVE_INFINITY:
        e.vroundsd(i.dest.reg, i.src1.reg, B00000010);
        break;
    }
  }
};
EMITTER(ROUND_V128, MATCH(I<OPCODE_ROUND, V128<>, V128<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    switch (i.instr->flags) {
      case ROUND_TO_ZERO:
        e.vroundps(i.dest.reg, i.src1.reg, B00000011);
        break;
      case ROUND_TO_NEAREST:
        e.vroundps(i.dest.reg, i.src1.reg, B00000000);
        break;
      case ROUND_TO_MINUS_INFINITY:
        e.vroundps(i.dest.reg, i.src1.reg, B00000001);
        break;
      case ROUND_TO_POSITIVE_INFINITY:
        e.vroundps(i.dest.reg, i.src1.reg, B00000010);
        break;
    }
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_ROUND,
    ROUND_F32::Select,
    ROUND_F64::Select,
    ROUND_V128::Select);


// ============================================================================
// OPCODE_VECTOR_CONVERT_I2F
// ============================================================================
EMITTER(VECTOR_CONVERT_I2F, MATCH(I<OPCODE_VECTOR_CONVERT_I2F, V128<>, V128<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // flags = ARITHMETIC_UNSIGNED
    // TODO(benvanik): are these really the same? VC++ thinks so.
    e.vcvtdq2ps(i.dest.reg, i.src1.reg);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_VECTOR_CONVERT_I2F,
    VECTOR_CONVERT_I2F::Select);


// ============================================================================
// OPCODE_VECTOR_CONVERT_F2I
// ============================================================================
EMITTER(VECTOR_CONVERT_F2I, MATCH(I<OPCODE_VECTOR_CONVERT_F2I, V128<>, V128<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // flags = ARITHMETIC_UNSIGNED | ARITHMETIC_UNSIGNED
    // TODO(benvanik): are these really the same? VC++ thinks so.
    e.vcvttps2dq(i.dest.reg, i.src1.reg);
    if (i.instr->flags & ARITHMETIC_SATURATE) {
      // TODO(benvanik): check saturation.
      e.UnimplementedInstr(i.instr);
    }
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_VECTOR_CONVERT_F2I,
    VECTOR_CONVERT_F2I::Select);


// ============================================================================
// OPCODE_LOAD_VECTOR_SHL
// ============================================================================
static vec128_t lvsl_table[17] = {
  vec128b( 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15),
  vec128b( 1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16),
  vec128b( 2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17),
  vec128b( 3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18),
  vec128b( 4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19),
  vec128b( 5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20),
  vec128b( 6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21),
  vec128b( 7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22),
  vec128b( 8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23),
  vec128b( 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24),
  vec128b(10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25),
  vec128b(11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26),
  vec128b(12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27),
  vec128b(13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28),
  vec128b(14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29),
  vec128b(15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30),
  vec128b(16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31),
};
EMITTER(LOAD_VECTOR_SHL_I8, MATCH(I<OPCODE_LOAD_VECTOR_SHL, V128<>, I8<>, V128<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
#if XE_DEBUG
    // We should only ever be getting values in [0,16]. Assert that.
    Xbyak::Label skip;
    e.cmp(i.src1.reg, 17);
    e.jb(skip);
    e.Trap();
    e.L(skip);
#endif  // XE_DEBUG
    // TODO(benvanik): find a cheaper way of doing this.
    e.movzx(e.rdx, i.src1.reg);
    e.shl(e.rdx, 4);
    e.mov(e.rax, (uintptr_t)lvsl_table);
    e.vmovaps(i.dest.reg, e.ptr[e.rax + e.rdx]);
  }
};
EMITTER(LOAD_VECTOR_SHL_I8C, MATCH(I<OPCODE_LOAD_VECTOR_SHL, V128<>, I8C<>, V128<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    auto sh = MIN(16, i.src1.constant);
    e.mov(e.rax, (uintptr_t)&lvsl_table[sh]);
    e.vmovaps(i.dest.reg, e.ptr[e.rax]);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_LOAD_VECTOR_SHL,
    LOAD_VECTOR_SHL_I8::Select,
    LOAD_VECTOR_SHL_I8C::Select);


// ============================================================================
// OPCODE_LOAD_VECTOR_SHR
// ============================================================================
static vec128_t lvsr_table[17] = {
  vec128b(16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31),
  vec128b(15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30),
  vec128b(14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29),
  vec128b(13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28),
  vec128b(12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27),
  vec128b(11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26),
  vec128b(10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25),
  vec128b( 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24),
  vec128b( 8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23),
  vec128b( 7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22),
  vec128b( 6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21),
  vec128b( 5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20),
  vec128b( 4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19),
  vec128b( 3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18),
  vec128b( 2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17),
  vec128b( 1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16),
  vec128b( 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15),
};
EMITTER(LOAD_VECTOR_SHR_I8, MATCH(I<OPCODE_LOAD_VECTOR_SHR, V128<>, I8<>, V128<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
#if XE_DEBUG
    // We should only ever be getting values in [0,16]. Assert that.
    Xbyak::Label skip;
    e.cmp(i.src1.reg, 17);
    e.jb(skip);
    e.Trap();
    e.L(skip);
#endif  // XE_DEBUG
    // TODO(benvanik): find a cheaper way of doing this.
    e.movzx(e.rdx, i.src1.reg);
    e.shl(e.rdx, 4);
    e.mov(e.rax, (uintptr_t)lvsr_table);
    e.vmovaps(i.dest.reg, e.ptr[e.rax + e.rdx]);
  }
};
EMITTER(LOAD_VECTOR_SHR_I8C, MATCH(I<OPCODE_LOAD_VECTOR_SHR, V128<>, I8C<>, V128<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    auto sh = MIN(16, i.src1.constant);
    e.mov(e.rax, (uintptr_t)&lvsr_table[sh]);
    e.vmovaps(i.dest.reg, e.ptr[e.rax]);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_LOAD_VECTOR_SHR,
    LOAD_VECTOR_SHR_I8::Select,
    LOAD_VECTOR_SHR_I8C::Select);


// ============================================================================
// OPCODE_LOAD_CLOCK
// ============================================================================
EMITTER(LOAD_CLOCK, MATCH(I<OPCODE_LOAD_CLOCK, I64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // It'd be cool to call QueryPerformanceCounter directly, but w/e.
    e.CallNative(LoadClock);
    e.mov(i.dest.reg, e.rax);
  }
  static uint64_t LoadClock(void* raw_context) {
    LARGE_INTEGER counter;
    uint64_t time = 0;
    if (QueryPerformanceCounter(&counter)) {
      time = counter.QuadPart;
    }
    return time;
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_LOAD_CLOCK,
    LOAD_CLOCK::Select);


// ============================================================================
// OPCODE_LOAD_LOCAL
// ============================================================================
// Note: all types are always aligned on the stack.
EMITTER(LOAD_LOCAL_I8, MATCH(I<OPCODE_LOAD_LOCAL, I8<>, I32C<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.mov(i.dest.reg, e.byte[e.rsp + i.src1.constant]);
    //e.TraceLoadI8(DATA_LOCAL, i.src1.constant, i.dest.reg);
  }
};
EMITTER(LOAD_LOCAL_I16, MATCH(I<OPCODE_LOAD_LOCAL, I16<>, I32C<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.mov(i.dest.reg, e.word[e.rsp + i.src1.constant]);
    //e.TraceLoadI16(DATA_LOCAL, i.src1.constant, i.dest.reg);
  }
};
EMITTER(LOAD_LOCAL_I32, MATCH(I<OPCODE_LOAD_LOCAL, I32<>, I32C<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.mov(i.dest.reg, e.dword[e.rsp + i.src1.constant]);
    //e.TraceLoadI32(DATA_LOCAL, i.src1.constant, i.dest.reg);
  }
};
EMITTER(LOAD_LOCAL_I64, MATCH(I<OPCODE_LOAD_LOCAL, I64<>, I32C<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.mov(i.dest.reg, e.qword[e.rsp + i.src1.constant]);
    //e.TraceLoadI64(DATA_LOCAL, i.src1.constant, i.dest.reg);
  }
};
EMITTER(LOAD_LOCAL_F32, MATCH(I<OPCODE_LOAD_LOCAL, F32<>, I32C<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vmovss(i.dest.reg, e.dword[e.rsp + i.src1.constant]);
    //e.TraceLoadF32(DATA_LOCAL, i.src1.constant, i.dest.reg);
  }
};
EMITTER(LOAD_LOCAL_F64, MATCH(I<OPCODE_LOAD_LOCAL, F64<>, I32C<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vmovsd(i.dest.reg, e.qword[e.rsp + i.src1.constant]);
    //e.TraceLoadF64(DATA_LOCAL, i.src1.constant, i.dest.reg);
  }
};
EMITTER(LOAD_LOCAL_V128, MATCH(I<OPCODE_LOAD_LOCAL, V128<>, I32C<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vmovaps(i.dest.reg, e.ptr[e.rsp + i.src1.constant]);
    //e.TraceLoadV128(DATA_LOCAL, i.src1.constant, i.dest.reg);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_LOAD_LOCAL,
    LOAD_LOCAL_I8::Select,
    LOAD_LOCAL_I16::Select,
    LOAD_LOCAL_I32::Select,
    LOAD_LOCAL_I64::Select,
    LOAD_LOCAL_F32::Select,
    LOAD_LOCAL_F64::Select,
    LOAD_LOCAL_V128::Select);


// ============================================================================
// OPCODE_STORE_LOCAL
// ============================================================================
// Note: all types are always aligned on the stack.
EMITTER(STORE_LOCAL_I8, MATCH(I<OPCODE_STORE_LOCAL, void, I32C<>, I8<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    //e.TraceStoreI8(DATA_LOCAL, i.src1.constant, i.src2.reg);
    e.mov(e.byte[e.rsp + i.src1.constant], i.src2.reg);
  }
};
EMITTER(STORE_LOCAL_I16, MATCH(I<OPCODE_STORE_LOCAL, void, I32C<>, I16<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    //e.TraceStoreI16(DATA_LOCAL, i.src1.constant, i.src2.reg);
    e.mov(e.word[e.rsp + i.src1.constant], i.src2.reg);
  }
};
EMITTER(STORE_LOCAL_I32, MATCH(I<OPCODE_STORE_LOCAL, void, I32C<>, I32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    //e.TraceStoreI32(DATA_LOCAL, i.src1.constant, i.src2.reg);
    e.mov(e.dword[e.rsp + i.src1.constant], i.src2.reg);
  }
};
EMITTER(STORE_LOCAL_I64, MATCH(I<OPCODE_STORE_LOCAL, void, I32C<>, I64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    //e.TraceStoreI64(DATA_LOCAL, i.src1.constant, i.src2.reg);
    e.mov(e.qword[e.rsp + i.src1.constant], i.src2.reg);
  }
};
EMITTER(STORE_LOCAL_F32, MATCH(I<OPCODE_STORE_LOCAL, void, I32C<>, F32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    //e.TraceStoreF32(DATA_LOCAL, i.src1.constant, i.src2.reg);
    e.vmovss(e.dword[e.rsp + i.src1.constant], i.src2.reg);
  }
};
EMITTER(STORE_LOCAL_F64, MATCH(I<OPCODE_STORE_LOCAL, void, I32C<>, F64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    //e.TraceStoreF64(DATA_LOCAL, i.src1.constant, i.src2.reg);
    e.vmovsd(e.qword[e.rsp + i.src1.constant], i.src2.reg);
  }
};
EMITTER(STORE_LOCAL_V128, MATCH(I<OPCODE_STORE_LOCAL, void, I32C<>, V128<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    //e.TraceStoreV128(DATA_LOCAL, i.src1.constant, i.src2.reg);
    e.vmovaps(e.ptr[e.rsp + i.src1.constant], i.src2.reg);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_STORE_LOCAL,
    STORE_LOCAL_I8::Select,
    STORE_LOCAL_I16::Select,
    STORE_LOCAL_I32::Select,
    STORE_LOCAL_I64::Select,
    STORE_LOCAL_F32::Select,
    STORE_LOCAL_F64::Select,
    STORE_LOCAL_V128::Select);


// ============================================================================
// OPCODE_LOAD_CONTEXT
// ============================================================================
// Note: all types are always aligned in the context.
EMITTER(LOAD_CONTEXT_I8, MATCH(I<OPCODE_LOAD_CONTEXT, I8<>, O64>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.mov(i.dest.reg, e.byte[e.rcx + i.src1.value]);
    //e.TraceLoadI8(DATA_CONTEXT, i.src1.value, i.dest.reg);
  }
};
EMITTER(LOAD_CONTEXT_I16, MATCH(I<OPCODE_LOAD_CONTEXT, I16<>, O64>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.mov(i.dest.reg, e.word[e.rcx + i.src1.value]);
    //e.TraceLoadI16(DATA_CONTEXT, i.src1.value, i.dest.reg);
  }
};
EMITTER(LOAD_CONTEXT_I32, MATCH(I<OPCODE_LOAD_CONTEXT, I32<>, O64>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.mov(i.dest.reg, e.dword[e.rcx + i.src1.value]);
    //e.TraceLoadI32(DATA_CONTEXT, i.src1.value, i.dest.reg);
  }
};
EMITTER(LOAD_CONTEXT_I64, MATCH(I<OPCODE_LOAD_CONTEXT, I64<>, O64>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.mov(i.dest.reg, e.qword[e.rcx + i.src1.value]);
    //e.TraceLoadI64(DATA_CONTEXT, i.src1.value, i.dest.reg);
  }
};
EMITTER(LOAD_CONTEXT_F32, MATCH(I<OPCODE_LOAD_CONTEXT, F32<>, O64>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vmovss(i.dest.reg, e.dword[e.rcx + i.src1.value]);
    //e.TraceLoadF32(DATA_CONTEXT, i.src1.value, i.dest.reg);
  }
};
EMITTER(LOAD_CONTEXT_F64, MATCH(I<OPCODE_LOAD_CONTEXT, F64<>, O64>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vmovsd(i.dest.reg, e.qword[e.rcx + i.src1.value]);
    //e.TraceLoadF64(DATA_CONTEXT, i.src1.value, i.dest.reg);
  }
};
EMITTER(LOAD_CONTEXT_V128, MATCH(I<OPCODE_LOAD_CONTEXT, V128<>, O64>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vmovaps(i.dest.reg, e.ptr[e.rcx + i.src1.value]);
    //e.TraceLoadV128(DATA_CONTEXT, i.src1.value, i.dest.reg);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_LOAD_CONTEXT,
    LOAD_CONTEXT_I8::Select,
    LOAD_CONTEXT_I16::Select,
    LOAD_CONTEXT_I32::Select,
    LOAD_CONTEXT_I64::Select,
    LOAD_CONTEXT_F32::Select,
    LOAD_CONTEXT_F64::Select,
    LOAD_CONTEXT_V128::Select);


// ============================================================================
// OPCODE_STORE_CONTEXT
// ============================================================================
// Note: all types are always aligned on the stack.
EMITTER(STORE_CONTEXT_I8, MATCH(I<OPCODE_STORE_CONTEXT, void, O64, I8<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    //e.TraceStoreI8(DATA_CONTEXT, i.src1.value, i.src2.reg);
    e.mov(e.byte[e.rcx + i.src1.value], i.src2.reg);
  }
};
EMITTER(STORE_CONTEXT_I16, MATCH(I<OPCODE_STORE_CONTEXT, void, O64, I16<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    //e.TraceStoreI16(DATA_CONTEXT, i.src1.value, i.src2.reg);
    e.mov(e.word[e.rcx + i.src1.value], i.src2.reg);
  }
};
EMITTER(STORE_CONTEXT_I32, MATCH(I<OPCODE_STORE_CONTEXT, void, O64, I32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    //e.TraceStoreI32(DATA_CONTEXT, i.src1.value, i.src2.reg);
    e.mov(e.dword[e.rcx + i.src1.value], i.src2.reg);
  }
};
EMITTER(STORE_CONTEXT_I64, MATCH(I<OPCODE_STORE_CONTEXT, void, O64, I64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    //e.TraceStoreI64(DATA_CONTEXT, i.src1.value, i.src2.reg);
    e.mov(e.qword[e.rcx + i.src1.value], i.src2.reg);
  }
};
EMITTER(STORE_CONTEXT_F32, MATCH(I<OPCODE_STORE_CONTEXT, void, O64, F32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    //e.TraceStoreF32(DATA_CONTEXT, i.src1.value, i.src2.reg);
    e.vmovss(e.dword[e.rcx + i.src1.value], i.src2.reg);
  }
};
EMITTER(STORE_CONTEXT_F64, MATCH(I<OPCODE_STORE_CONTEXT, void, O64, F64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    //e.TraceStoreF64(DATA_CONTEXT, i.src1.value, i.src2.reg);
    e.vmovsd(e.qword[e.rcx + i.src1.value], i.src2.reg);
  }
};
EMITTER(STORE_CONTEXT_V128, MATCH(I<OPCODE_STORE_CONTEXT, void, O64, V128<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    //e.TraceStoreV128(DATA_CONTEXT, i.src1.value, i.src2.reg);
    e.vmovaps(e.ptr[e.rcx + i.src1.value], i.src2.reg);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_STORE_CONTEXT,
    STORE_CONTEXT_I8::Select,
    STORE_CONTEXT_I16::Select,
    STORE_CONTEXT_I32::Select,
    STORE_CONTEXT_I64::Select,
    STORE_CONTEXT_F32::Select,
    STORE_CONTEXT_F64::Select,
    STORE_CONTEXT_V128::Select);


// ============================================================================
// OPCODE_LOAD
// ============================================================================
// Note: most *should* be aligned, but needs to be checked!
RegExp ComputeMemoryAddress(X64Emitter& e, Reg64 guest) {
  return e.rdx + guest;
}
EMITTER(LOAD_I8, MATCH(I<OPCODE_LOAD, I8<>, I64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    //e.TraceMemoryLoadI8(i.src1.reg);
    e.mov(i.dest.reg, e.byte[ComputeMemoryAddress(e, i.src1.reg)]);
  }
};
EMITTER(LOAD_I16, MATCH(I<OPCODE_LOAD, I16<>, I64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    //e.TraceMemoryLoadI16(i.src1.reg);
    e.mov(i.dest.reg, e.word[ComputeMemoryAddress(e, i.src1.reg)]);
  }
};
EMITTER(LOAD_I32, MATCH(I<OPCODE_LOAD, I32<>, I64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    //e.TraceMemoryLoadI32(i.src1.reg);
    e.mov(i.dest.reg, e.dword[ComputeMemoryAddress(e, i.src1.reg)]);
  }
};
EMITTER(LOAD_I64, MATCH(I<OPCODE_LOAD, I64<>, I64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    //e.TraceMemoryLoadI64(i.src1.reg);
    e.mov(i.dest.reg, e.qword[ComputeMemoryAddress(e, i.src1.reg)]);
  }
};
EMITTER(LOAD_F32, MATCH(I<OPCODE_LOAD, F32<>, I64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    //e.TraceMemoryLoadF32(i.src1.reg);
    e.vmovss(i.dest.reg, e.dword[ComputeMemoryAddress(e, i.src1.reg)]);
  }
};
EMITTER(LOAD_F64, MATCH(I<OPCODE_LOAD, F64<>, I64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    //e.TraceMemoryLoadF64(i.src1.reg);
    e.vmovsd(i.dest.reg, e.qword[ComputeMemoryAddress(e, i.src1.reg)]);
  }
};
EMITTER(LOAD_V128, MATCH(I<OPCODE_LOAD, V128<>, I64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // TODO(benvanik): we should try to stick to movaps if possible.
    //e.TraceMemoryLoadV128(i.src1.reg);
    e.vmovups(i.dest.reg, e.ptr[ComputeMemoryAddress(e, i.src1.reg)]);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_LOAD,
    LOAD_I8::Select,
    LOAD_I16::Select,
    LOAD_I32::Select,
    LOAD_I64::Select,
    LOAD_F32::Select,
    LOAD_F64::Select,
    LOAD_V128::Select);


// ============================================================================
// OPCODE_STORE
// ============================================================================
// Note: most *should* be aligned, but needs to be checked!
EMITTER(STORE_I8, MATCH(I<OPCODE_STORE, void, I64<>, I8<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    //e.TraceMemoryStoreI8(i.src1.reg, i.src2.reg);
    e.mov(e.byte[ComputeMemoryAddress(e, i.src1.reg)], i.src2.reg);
  }
};
EMITTER(STORE_I16, MATCH(I<OPCODE_STORE, void, I64<>, I16<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    //e.TraceMemoryStoreI16(i.src1.reg, i.src2.reg);
    e.mov(e.word[ComputeMemoryAddress(e, i.src1.reg)], i.src2.reg);
  }
};
EMITTER(STORE_I32, MATCH(I<OPCODE_STORE, void, I64<>, I32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    //e.TraceMemoryStoreI32(i.src1.reg, i.src2.reg);
    e.mov(e.dword[ComputeMemoryAddress(e, i.src1.reg)], i.src2.reg);
  }
};
EMITTER(STORE_I64, MATCH(I<OPCODE_STORE, void, I64<>, I64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    //e.TraceMemoryStoreI64(i.src1.reg, i.src2.reg);
    e.mov(e.qword[ComputeMemoryAddress(e, i.src1.reg)], i.src2.reg);
  }
};
EMITTER(STORE_F32, MATCH(I<OPCODE_STORE, void, I64<>, F32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    //e.TraceMemoryStoreF32(i.src1.reg, i.src2.reg);
    e.vmovss(e.dword[ComputeMemoryAddress(e, i.src1.reg)], i.src2.reg);
  }
};
EMITTER(STORE_F64, MATCH(I<OPCODE_STORE, void, I64<>, F64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    //e.TraceMemoryStoreF64(i.src1.reg, i.src2.reg);
    e.vmovsd(e.qword[ComputeMemoryAddress(e, i.src1.reg)], i.src2.reg);
  }
};
EMITTER(STORE_V128, MATCH(I<OPCODE_STORE, void, I64<>, V128<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    //e.TraceMemoryStoreV128(i.src1.reg, i.src2.reg);
    e.vmovaps(e.ptr[ComputeMemoryAddress(e, i.src1.reg)], i.src2.reg);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_STORE,
    STORE_I8::Select,
    STORE_I16::Select,
    STORE_I32::Select,
    STORE_I64::Select,
    STORE_F32::Select,
    STORE_F64::Select,
    STORE_V128::Select);


// ============================================================================
// OPCODE_PREFETCH
// ============================================================================
EMITTER(PREFETCH, MATCH(I<OPCODE_PREFETCH, void, I64<>, O64>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // TODO(benvanik): prefetch addr -> length.
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_PREFETCH,
    PREFETCH::Select);


// ============================================================================
// OPCODE_MAX
// ============================================================================
EMITTER(MAX_F32, MATCH(I<OPCODE_MAX, F32<>, F32<>, F32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vmaxss(i.dest.reg, i.src1.reg, i.src2.reg);
  }
};
EMITTER(MAX_F64, MATCH(I<OPCODE_MAX, F64<>, F64<>, F64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vmaxsd(i.dest.reg, i.src1.reg, i.src2.reg);
  }
};
EMITTER(MAX_V128, MATCH(I<OPCODE_MAX, V128<>, V128<>, V128<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vmaxps(i.dest.reg, i.src1.reg, i.src2.reg);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_MAX,
    MAX_F32::Select,
    MAX_F64::Select,
    MAX_V128::Select);


// ============================================================================
// OPCODE_MIN
// ============================================================================
EMITTER(MIN_F32, MATCH(I<OPCODE_MIN, F32<>, F32<>, F32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vminss(i.dest.reg, i.src1.reg, i.src2.reg);
  }
};
EMITTER(MIN_F64, MATCH(I<OPCODE_MIN, F64<>, F64<>, F64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vminsd(i.dest.reg, i.src1.reg, i.src2.reg);
  }
};
EMITTER(MIN_V128, MATCH(I<OPCODE_MIN, V128<>, V128<>, V128<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vminps(i.dest.reg, i.src1.reg, i.src2.reg);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_MIN,
    MIN_F32::Select,
    MIN_F64::Select,
    MIN_V128::Select);


// ============================================================================
// OPCODE_SELECT
// ============================================================================
EMITTER(SELECT_I8, MATCH(I<OPCODE_SELECT, I8<>, I8<>, I8<>, I8<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.test(i.src1.reg, i.src1.reg);
    e.cmovnz(i.dest.reg.cvt32(), i.src2.reg.cvt32());
    e.cmovz(i.dest.reg.cvt32(), i.src3.reg.cvt32());
  }
};
EMITTER(SELECT_I16, MATCH(I<OPCODE_SELECT, I16<>, I8<>, I16<>, I16<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.test(i.src1.reg, i.src1.reg);
    e.cmovnz(i.dest.reg.cvt32(), i.src2.reg.cvt32());
    e.cmovz(i.dest.reg.cvt32(), i.src3.reg.cvt32());
  }
};
EMITTER(SELECT_I32, MATCH(I<OPCODE_SELECT, I32<>, I8<>, I32<>, I32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.test(i.src1.reg, i.src1.reg);
    e.cmovnz(i.dest.reg, i.src2.reg);
    e.cmovz(i.dest.reg, i.src3.reg);
  }
};
EMITTER(SELECT_I64, MATCH(I<OPCODE_SELECT, I64<>, I8<>, I64<>, I64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.test(i.src1.reg, i.src1.reg);
    e.cmovnz(i.dest.reg, i.src2.reg);
    e.cmovz(i.dest.reg, i.src3.reg);
  }
};
EMITTER(SELECT_F32, MATCH(I<OPCODE_SELECT, F32<>, I8<>, F32<>, F32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.test(i.src1.reg, i.src1.reg);
    // TODO(benvanik): find a way to do this without branches.
    Xbyak::Label skip;
    e.vmovaps(i.dest.reg, i.src3.reg);
    e.jz(skip);
    e.vmovaps(i.dest.reg, i.src2.reg);
    e.L(skip);
  }
};
EMITTER(SELECT_F64, MATCH(I<OPCODE_SELECT, F64<>, I8<>, F64<>, F64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.test(i.src1.reg, i.src1.reg);
    // TODO(benvanik): find a way to do this without branches.
    Xbyak::Label skip;
    e.vmovaps(i.dest.reg, i.src3.reg);
    e.jz(skip);
    e.vmovaps(i.dest.reg, i.src2.reg);
    e.L(skip);
  }
};
EMITTER(SELECT_V128, MATCH(I<OPCODE_SELECT, V128<>, I8<>, V128<>, V128<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.test(i.src1.reg, i.src1.reg);
    // TODO(benvanik): find a way to do this without branches.
    Xbyak::Label skip;
    e.vmovaps(i.dest.reg, i.src3.reg);
    e.jz(skip);
    e.vmovaps(i.dest.reg, i.src2.reg);
    e.L(skip);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_SELECT,
    SELECT_I8::Select,
    SELECT_I16::Select,
    SELECT_I32::Select,
    SELECT_I64::Select,
    SELECT_F32::Select,
    SELECT_F64::Select,
    SELECT_V128::Select);


// ============================================================================
// OPCODE_IS_TRUE
// ============================================================================
EMITTER(IS_TRUE_I8, MATCH(I<OPCODE_IS_TRUE, I8<>, I8<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.test(i.src1.reg, i.src1.reg);
    e.setnz(i.dest.reg);
  }
};
EMITTER(IS_TRUE_I16, MATCH(I<OPCODE_IS_TRUE, I8<>, I16<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.test(i.src1.reg, i.src1.reg);
    e.setnz(i.dest.reg);
  }
};
EMITTER(IS_TRUE_I32, MATCH(I<OPCODE_IS_TRUE, I8<>, I32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.test(i.src1.reg, i.src1.reg);
    e.setnz(i.dest.reg);
  }
};
EMITTER(IS_TRUE_I64, MATCH(I<OPCODE_IS_TRUE, I8<>, I64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.test(i.src1.reg, i.src1.reg);
    e.setnz(i.dest.reg);
  }
};
EMITTER(IS_TRUE_F32, MATCH(I<OPCODE_IS_TRUE, I8<>, F32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vptest(i.src1.reg, i.src1.reg);
    e.setnz(i.dest.reg);
  }
};
EMITTER(IS_TRUE_F64, MATCH(I<OPCODE_IS_TRUE, I8<>, F64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vptest(i.src1.reg, i.src1.reg);
    e.setnz(i.dest.reg);
  }
};
EMITTER(IS_TRUE_V128, MATCH(I<OPCODE_IS_TRUE, I8<>, V128<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vptest(i.src1.reg, i.src1.reg);
    e.setnz(i.dest.reg);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_IS_TRUE,
    IS_TRUE_I8::Select,
    IS_TRUE_I16::Select,
    IS_TRUE_I32::Select,
    IS_TRUE_I64::Select,
    IS_TRUE_F32::Select,
    IS_TRUE_F64::Select,
    IS_TRUE_V128::Select);


// ============================================================================
// OPCODE_IS_FALSE
// ============================================================================
EMITTER(IS_FALSE_I8, MATCH(I<OPCODE_IS_FALSE, I8<>, I8<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.test(i.src1.reg, i.src1.reg);
    e.setz(i.dest.reg);
  }
};
EMITTER(IS_FALSE_I16, MATCH(I<OPCODE_IS_FALSE, I8<>, I16<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.test(i.src1.reg, i.src1.reg);
    e.setz(i.dest.reg);
  }
};
EMITTER(IS_FALSE_I32, MATCH(I<OPCODE_IS_FALSE, I8<>, I32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.test(i.src1.reg, i.src1.reg);
    e.setz(i.dest.reg);
  }
};
EMITTER(IS_FALSE_I64, MATCH(I<OPCODE_IS_FALSE, I8<>, I64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.test(i.src1.reg, i.src1.reg);
    e.setz(i.dest.reg);
  }
};
EMITTER(IS_FALSE_F32, MATCH(I<OPCODE_IS_FALSE, I8<>, F32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vptest(i.src1.reg, i.src1.reg);
    e.setz(i.dest.reg);
  }
};
EMITTER(IS_FALSE_F64, MATCH(I<OPCODE_IS_FALSE, I8<>, F64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vptest(i.src1.reg, i.src1.reg);
    e.setz(i.dest.reg);
  }
};
EMITTER(IS_FALSE_V128, MATCH(I<OPCODE_IS_FALSE, I8<>, V128<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vptest(i.src1.reg, i.src1.reg);
    e.setz(i.dest.reg);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_IS_FALSE,
    IS_FALSE_I8::Select,
    IS_FALSE_I16::Select,
    IS_FALSE_I32::Select,
    IS_FALSE_I64::Select,
    IS_FALSE_F32::Select,
    IS_FALSE_F64::Select,
    IS_FALSE_V128::Select);


// ============================================================================
// OPCODE_COMPARE_*
// ============================================================================
#define EMITTER_COMPARE_XX(op, instr) \
    EMITTER(COMPARE_##op##_I8, MATCH(I<OPCODE_COMPARE_##op##, I8<>, I8<>, I8<>>)) { \
      static void Emit(X64Emitter& e, const EmitArgType& i) { \
        e.cmp(i.src1.reg, i.src2.reg); \
        e.instr(i.dest.reg); \
      } \
    }; \
    EMITTER(COMPARE_##op##_I16, MATCH(I<OPCODE_COMPARE_##op##, I8<>, I16<>, I16<>>)) { \
      static void Emit(X64Emitter& e, const EmitArgType& i) { \
        e.cmp(i.src1.reg, i.src2.reg); \
        e.instr(i.dest.reg); \
      } \
    }; \
    EMITTER(COMPARE_##op##_I32, MATCH(I<OPCODE_COMPARE_##op##, I8<>, I32<>, I32<>>)) { \
      static void Emit(X64Emitter& e, const EmitArgType& i) { \
        e.cmp(i.src1.reg, i.src2.reg); \
        e.instr(i.dest.reg); \
      } \
    }; \
    EMITTER(COMPARE_##op##_I64, MATCH(I<OPCODE_COMPARE_##op##, I8<>, I64<>, I64<>>)) { \
      static void Emit(X64Emitter& e, const EmitArgType& i) { \
        e.cmp(i.src1.reg, i.src2.reg); \
        e.instr(i.dest.reg); \
      } \
    }; \
    EMITTER(COMPARE_##op##_F32, MATCH(I<OPCODE_COMPARE_##op##, I8<>, F32<>, F32<>>)) { \
      static void Emit(X64Emitter& e, const EmitArgType& i) { \
        e.vcomiss(i.src1.reg, i.src2.reg); \
        e.instr(i.dest.reg); \
      } \
    }; \
    EMITTER(COMPARE_##op##_F64, MATCH(I<OPCODE_COMPARE_##op##, I8<>, F64<>, F64<>>)) { \
      static void Emit(X64Emitter& e, const EmitArgType& i) { \
        e.vcomisd(i.src1.reg, i.src2.reg); \
        e.instr(i.dest.reg); \
      } \
    }; \
    EMITTER_OPCODE_TABLE( \
        OPCODE_COMPARE_##op##, \
        COMPARE_##op##_I8::Select, \
        COMPARE_##op##_I16::Select, \
        COMPARE_##op##_I32::Select, \
        COMPARE_##op##_I64::Select, \
        COMPARE_##op##_F32::Select, \
        COMPARE_##op##_F64::Select);
EMITTER_COMPARE_XX(EQ, sete);
EMITTER_COMPARE_XX(NE, setne);
EMITTER_COMPARE_XX(SLT, setl);
EMITTER_COMPARE_XX(SLE, setle);
EMITTER_COMPARE_XX(SGT, setg);
EMITTER_COMPARE_XX(SGE, setge);
EMITTER_COMPARE_XX(ULT, setb);
EMITTER_COMPARE_XX(ULE, setbe);
EMITTER_COMPARE_XX(UGT, seta);
EMITTER_COMPARE_XX(UGE, setae);


// ============================================================================
// OPCODE_DID_CARRY
// ============================================================================
EMITTER(DID_CARRY, MATCH(I<OPCODE_DID_CARRY, I8<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.LoadEflags();
    e.setc(i.dest.reg);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_DID_CARRY,
    DID_CARRY::Select);


// ============================================================================
// OPCODE_DID_OVERFLOW
// ============================================================================
EMITTER(DID_OVERFLOW, MATCH(I<OPCODE_DID_OVERFLOW, I8<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.LoadEflags();
    e.seto(i.dest.reg);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_DID_OVERFLOW,
    DID_OVERFLOW::Select);


// ============================================================================
// OPCODE_DID_SATURATE
// ============================================================================
//EMITTER(DID_SATURATE, MATCH(I<OPCODE_DID_SATURATE, I8<>>)) {
//  static void Emit(X64Emitter& e, const EmitArgType& i) {
//  }
//};
//EMITTER_OPCODE_TABLE(OPCODE_DID_SATURATE,
//                     DID_SATURATE);


// ============================================================================
// OPCODE_VECTOR_COMPARE_EQ
// ============================================================================
EMITTER(VECTOR_COMPARE_EQ_V128, MATCH(I<OPCODE_VECTOR_COMPARE_EQ, V128<>, V128<>, V128<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    switch (i.instr->flags) {
    case INT8_TYPE:
      e.vpcmpeqb(i.dest.reg, i.src1.reg, i.src2.reg);
      break;
    case INT16_TYPE:
      e.vpcmpeqw(i.dest.reg, i.src1.reg, i.src2.reg);
      break;
    case INT32_TYPE:
      e.vpcmpeqd(i.dest.reg, i.src1.reg, i.src2.reg);
      break;
    case FLOAT32_TYPE:
      e.vcmpeqps(i.dest.reg, i.src1.reg, i.src2.reg);
      break;
    }
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_VECTOR_COMPARE_EQ,
    VECTOR_COMPARE_EQ_V128::Select);


// ============================================================================
// OPCODE_VECTOR_COMPARE_SGT
// ============================================================================
EMITTER(VECTOR_COMPARE_SGT_V128, MATCH(I<OPCODE_VECTOR_COMPARE_SGT, V128<>, V128<>, V128<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    switch (i.instr->flags) {
    case INT8_TYPE:
      e.vpcmpgtb(i.dest.reg, i.src1.reg, i.src2.reg);
      break;
    case INT16_TYPE:
      e.vpcmpgtw(i.dest.reg, i.src1.reg, i.src2.reg);
      break;
    case INT32_TYPE:
      e.vpcmpgtd(i.dest.reg, i.src1.reg, i.src2.reg);
      break;
    case FLOAT32_TYPE:
      e.vcmpgtps(i.dest.reg, i.src1.reg, i.src2.reg);
      break;
    }
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_VECTOR_COMPARE_SGT,
    VECTOR_COMPARE_SGT_V128::Select);


// ============================================================================
// OPCODE_VECTOR_COMPARE_SGE
// ============================================================================
EMITTER(VECTOR_COMPARE_SGE_V128, MATCH(I<OPCODE_VECTOR_COMPARE_SGE, V128<>, V128<>, V128<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    switch (i.instr->flags) {
    case INT8_TYPE:
      e.vpcmpgtb(i.dest.reg, i.src1.reg, i.src2.reg);
      e.vpcmpeqb(e.xmm0, i.src1.reg, i.src2.reg);
      e.vpor(i.dest.reg, e.xmm0);
      break;
    case INT16_TYPE:
      e.vpcmpgtw(i.dest.reg, i.src1.reg, i.src2.reg);
      e.vpcmpeqw(e.xmm0, i.src1.reg, i.src2.reg);
      e.vpor(i.dest.reg, e.xmm0);
      break;
    case INT32_TYPE:
      e.vpcmpgtd(i.dest.reg, i.src1.reg, i.src2.reg);
      e.vpcmpeqd(e.xmm0, i.src1.reg, i.src2.reg);
      e.vpor(i.dest.reg, e.xmm0);
      break;
    case FLOAT32_TYPE:
      e.vcmpgeps(i.dest.reg, i.src1.reg, i.src2.reg);
      break;
    }
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_VECTOR_COMPARE_SGE,
    VECTOR_COMPARE_SGE_V128::Select);


// ============================================================================
// OPCODE_VECTOR_COMPARE_UGT
// ============================================================================
//EMITTER(VECTOR_COMPARE_UGT_V128, MATCH(I<OPCODE_VECTOR_COMPARE_UGT, V128<>, V128<>, V128<>>)) {
//  static void Emit(X64Emitter& e, const EmitArgType& i) {
//  }
//};
//EMITTER_OPCODE_TABLE(
//    OPCODE_VECTOR_COMPARE_UGT,
//    VECTOR_COMPARE_UGT_V128::Select);


// ============================================================================
// OPCODE_VECTOR_COMPARE_UGE
// ============================================================================
//EMITTER(VECTOR_COMPARE_UGE_V128, MATCH(I<OPCODE_VECTOR_COMPARE_UGE, V128<>, V128<>, V128<>>)) {
//  static void Emit(X64Emitter& e, const EmitArgType& i) {
//  }
//};
//EMITTER_OPCODE_TABLE(
//    OPCODE_VECTOR_COMPARE_UGE,
//    VECTOR_COMPARE_UGE_V128::Select);


// ============================================================================
// OPCODE_ADD
// ============================================================================
EMITTER(ADD_I8, MATCH(I<OPCODE_ADD, I8<>, I8<>, I8<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // TODO(benvanik): put dest/src1|2 together.
    if (i.dest.reg.getIdx() == i.src1.reg.getIdx()) {
      e.add(i.dest.reg, i.src2.reg);
    } else if (i.dest.reg.getIdx() == i.src2.reg.getIdx()) {
      e.add(i.dest.reg, i.src1.reg);
    } else {
      e.mov(i.dest.reg, i.src1.reg);
      e.add(i.dest.reg, i.src2.reg);
    }
    if (i.instr->flags & ARITHMETIC_SET_CARRY) {
      // CF is set if carried.
      e.StoreEflags();
    }
  }
};
EMITTER(ADD_I16, MATCH(I<OPCODE_ADD, I16<>, I16<>, I16<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // TODO(benvanik): put dest/src1|2 together.
    if (i.dest.reg.getIdx() == i.src1.reg.getIdx()) {
      e.add(i.dest.reg, i.src2.reg);
    } else if (i.dest.reg.getIdx() == i.src2.reg.getIdx()) {
      e.add(i.dest.reg, i.src1.reg);
    } else {
      e.mov(i.dest.reg, i.src1.reg);
      e.add(i.dest.reg, i.src2.reg);
    }
    if (i.instr->flags & ARITHMETIC_SET_CARRY) {
      // CF is set if carried.
      e.StoreEflags();
    }
  }
};
EMITTER(ADD_I32, MATCH(I<OPCODE_ADD, I32<>, I32<>, I32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // TODO(benvanik): put dest/src1|2 together.
    if (i.dest.reg.getIdx() == i.src1.reg.getIdx()) {
      e.add(i.dest.reg, i.src2.reg);
    } else if (i.dest.reg.getIdx() == i.src2.reg.getIdx()) {
      e.add(i.dest.reg, i.src1.reg);
    } else {
      e.mov(i.dest.reg, i.src1.reg);
      e.add(i.dest.reg, i.src2.reg);
    }
    if (i.instr->flags & ARITHMETIC_SET_CARRY) {
      // CF is set if carried.
      e.StoreEflags();
    }
  }
};
EMITTER(ADD_I64, MATCH(I<OPCODE_ADD, I64<>, I64<>, I64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // TODO(benvanik): put dest/src1|2 together.
    if (i.dest.reg.getIdx() == i.src1.reg.getIdx()) {
      e.add(i.dest.reg, i.src2.reg);
    } else if (i.dest.reg.getIdx() == i.src2.reg.getIdx()) {
      e.add(i.dest.reg, i.src1.reg);
    } else {
      e.mov(i.dest.reg, i.src1.reg);
      e.add(i.dest.reg, i.src2.reg);
    }
    if (i.instr->flags & ARITHMETIC_SET_CARRY) {
      // CF is set if carried.
      e.StoreEflags();
    }
  }
};
EMITTER(ADD_F32, MATCH(I<OPCODE_ADD, F32<>, F32<>, F32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vaddss(i.dest.reg, i.src1.reg, i.src2.reg);
  }
};
EMITTER(ADD_F64, MATCH(I<OPCODE_ADD, F64<>, F64<>, F64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vaddsd(i.dest.reg, i.src1.reg, i.src2.reg);
  }
};
EMITTER(ADD_V128, MATCH(I<OPCODE_ADD, V128<>, V128<>, V128<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vaddps(i.dest.reg, i.src1.reg, i.src2.reg);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_ADD,
    ADD_I8::Select,
    ADD_I16::Select,
    ADD_I32::Select,
    ADD_I64::Select,
    ADD_F32::Select,
    ADD_F64::Select,
    ADD_V128::Select);


// ============================================================================
// OPCODE_ADD_CARRY
// ============================================================================


// ============================================================================
// OPCODE_VECTOR_ADD
// ============================================================================


// ============================================================================
// OPCODE_SUB
// ============================================================================
EMITTER(SUB_I8, MATCH(I<OPCODE_SUB, I8<>, I8<>, I8<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // TODO(benvanik): put dest/src1 together.
    if (i.dest.reg.getIdx() == i.src1.reg.getIdx()) {
      e.sub(i.dest.reg, i.src2.reg);
    } else {
      e.mov(i.dest.reg, i.src1.reg);
      e.sub(i.dest.reg, i.src2.reg);
    }
    if (i.instr->flags & ARITHMETIC_SET_CARRY) {
      // TODO(benvanik): set flags?
      e.UnimplementedInstr(i.instr);
      e.StoreEflags();
    }
  }
};
EMITTER(SUB_I16, MATCH(I<OPCODE_SUB, I16<>, I16<>, I16<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // TODO(benvanik): put dest/src1 together.
    if (i.dest.reg.getIdx() == i.src1.reg.getIdx()) {
      e.sub(i.dest.reg, i.src2.reg);
    } else {
      e.mov(i.dest.reg, i.src1.reg);
      e.sub(i.dest.reg, i.src2.reg);
    }
    if (i.instr->flags & ARITHMETIC_SET_CARRY) {
      // TODO(benvanik): set flags?
      e.UnimplementedInstr(i.instr);
      e.StoreEflags();
    }
  }
};
EMITTER(SUB_I32, MATCH(I<OPCODE_SUB, I32<>, I32<>, I32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // TODO(benvanik): put dest/src1 together.
    if (i.dest.reg.getIdx() == i.src1.reg.getIdx()) {
      e.sub(i.dest.reg, i.src2.reg);
    } else {
      e.mov(i.dest.reg, i.src1.reg);
      e.sub(i.dest.reg, i.src2.reg);
    }
    if (i.instr->flags & ARITHMETIC_SET_CARRY) {
      // TODO(benvanik): set flags?
      e.UnimplementedInstr(i.instr);
      e.StoreEflags();
    }
  }
};
EMITTER(SUB_I64, MATCH(I<OPCODE_SUB, I64<>, I64<>, I64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // TODO(benvanik): put dest/src1 together.
    if (i.dest.reg.getIdx() == i.src1.reg.getIdx()) {
      e.sub(i.dest.reg, i.src2.reg);
    } else {
      e.mov(i.dest.reg, i.src1.reg);
      e.sub(i.dest.reg, i.src2.reg);
    }
    if (i.instr->flags & ARITHMETIC_SET_CARRY) {
      // TODO(benvanik): set flags?
      e.UnimplementedInstr(i.instr);
      e.StoreEflags();
    }
  }
};
EMITTER(SUB_F32, MATCH(I<OPCODE_SUB, F32<>, F32<>, F32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    XEASSERT(!i.instr->flags);
    e.vsubss(i.dest.reg, i.src1.reg, i.src2.reg);
  }
};
EMITTER(SUB_F64, MATCH(I<OPCODE_SUB, F64<>, F64<>, F64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    XEASSERT(!i.instr->flags);
    e.vsubsd(i.dest.reg, i.src1.reg, i.src2.reg);
  }
};
EMITTER(SUB_V128, MATCH(I<OPCODE_SUB, V128<>, V128<>, V128<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    XEASSERT(!i.instr->flags);
    e.vsubps(i.dest.reg, i.src1.reg, i.src2.reg);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_SUB,
    SUB_I8::Select,
    SUB_I16::Select,
    SUB_I32::Select,
    SUB_I64::Select,
    SUB_F32::Select,
    SUB_F64::Select,
    SUB_V128::Select);


// ============================================================================
// OPCODE_MUL
// ============================================================================
// Sign doesn't matter here, as we don't use the high bits.
// We exploit mulx here to avoid creating too much register pressure.
EMITTER(MUL_I8, MATCH(I<OPCODE_MUL, I8<>, I8<>, I8<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // dest hi, dest low = src * edx
    // TODO(benvanik): place src2 in edx?
    e.movzx(e.edx, i.src2.reg);
    e.mulx(i.dest.reg.cvt32(), i.dest.reg.cvt32(), i.src1.reg.cvt32());
  }
};
EMITTER(MUL_I16, MATCH(I<OPCODE_MUL, I16<>, I16<>, I16<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // dest hi, dest low = src * edx
    // TODO(benvanik): place src2 in edx?
    e.movzx(e.edx, i.src2.reg);
    e.mulx(i.dest.reg.cvt32(), i.dest.reg.cvt32(), i.src1.reg.cvt32());
  }
};
EMITTER(MUL_I32, MATCH(I<OPCODE_MUL, I32<>, I32<>, I32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // dest hi, dest low = src * edx
    // TODO(benvanik): place src2 in edx?
    e.mov(e.edx, i.src2.reg);
    e.mulx(i.dest.reg, i.dest.reg, i.src1.reg);
  }
};
EMITTER(MUL_I64, MATCH(I<OPCODE_MUL, I64<>, I64<>, I64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // dest hi, dest low = src * edx
    // TODO(benvanik): place src2 in edx?
    e.mov(e.edx, i.src2.reg);
    e.mulx(i.dest.reg, i.dest.reg, i.src1.reg);
  }
};
EMITTER(MUL_F32, MATCH(I<OPCODE_MUL, F32<>, F32<>, F32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    XEASSERT(!i.instr->flags);
    e.vmulss(i.dest.reg, i.src1.reg, i.src2.reg);
  }
};
EMITTER(MUL_F64, MATCH(I<OPCODE_MUL, F64<>, F64<>, F64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    XEASSERT(!i.instr->flags);
    e.vmulsd(i.dest.reg, i.src1.reg, i.src2.reg);
  }
};
EMITTER(MUL_V128, MATCH(I<OPCODE_MUL, V128<>, V128<>, V128<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    XEASSERT(!i.instr->flags);
    e.vmulps(i.dest.reg, i.src1.reg, i.src2.reg);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_MUL,
    MUL_I8::Select,
    MUL_I16::Select,
    MUL_I32::Select,
    MUL_I64::Select,
    MUL_F32::Select,
    MUL_F64::Select,
    MUL_V128::Select);


// ============================================================================
// OPCODE_MUL_HI
// ============================================================================
EMITTER(MUL_HI_I8, MATCH(I<OPCODE_MUL_HI, I8<>, I8<>, I8<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    if (i.instr->flags & ARITHMETIC_UNSIGNED) {
      // TODO(benvanik): place src1 in eax? still need to sign extend
      e.movzx(e.eax, i.src1.reg);
      e.mulx(i.dest.reg.cvt32(), e.eax, i.src2.reg.cvt32());
    } else {
      e.mov(e.al, i.src1.reg);
      e.imul(i.src2.reg);
      e.mov(i.dest.reg, e.ah);
    }
  }
};
EMITTER(MUL_HI_I16, MATCH(I<OPCODE_MUL_HI, I16<>, I16<>, I16<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    if (i.instr->flags & ARITHMETIC_UNSIGNED) {
      // TODO(benvanik): place src1 in eax? still need to sign extend
      e.movzx(e.eax, i.src1.reg);
      e.mulx(i.dest.reg.cvt32(), e.eax, i.src2.reg.cvt32());
    } else {
      e.mov(e.ax, i.src1.reg);
      e.imul(i.src2.reg);
      e.mov(i.dest.reg, e.dx);
    }
  }
};
EMITTER(MUL_HI_I32, MATCH(I<OPCODE_MUL_HI, I32<>, I32<>, I32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    if (i.instr->flags & ARITHMETIC_UNSIGNED) {
      // TODO(benvanik): place src1 in eax? still need to sign extend
      e.mov(e.eax, i.src1.reg);
      e.mulx(i.dest.reg, e.eax, i.src2.reg);
    } else {
      e.mov(e.eax, i.src1.reg);
      e.imul(i.src2.reg);
      e.mov(i.dest.reg, e.edx);
    }
  }
};
EMITTER(MUL_HI_I64, MATCH(I<OPCODE_MUL_HI, I64<>, I64<>, I64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    if (i.instr->flags & ARITHMETIC_UNSIGNED) {
      // TODO(benvanik): place src1 in eax? still need to sign extend
      e.mov(e.rax, i.src1.reg);
      e.mulx(i.dest.reg, e.rax, i.src2.reg);
    } else {
      e.mov(e.rax, i.src1.reg);
      e.imul(i.src2.reg);
      e.mov(i.dest.reg, e.rdx);
    }
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_MUL_HI,
    MUL_HI_I8::Select,
    MUL_HI_I16::Select,
    MUL_HI_I32::Select,
    MUL_HI_I64::Select);


// ============================================================================
// OPCODE_DIV
// ============================================================================
EMITTER(DIV_I8, MATCH(I<OPCODE_DIV, I8<>, I8<>, I8<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // NOTE: RDX clobbered.
    if (i.instr->flags & ARITHMETIC_UNSIGNED) {
      e.movzx(e.ax, i.src1.reg);
      e.div(i.src2.reg);
    } else {
      e.movsx(e.ax, i.src1.reg);
      e.idiv(i.src2.reg);
    }
    e.mov(i.dest.reg, e.al);
  }
};
EMITTER(DIV_I16, MATCH(I<OPCODE_DIV, I16<>, I16<>, I16<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // NOTE: RDX clobbered.
    e.mov(e.ax, i.src1.reg);
    if (i.instr->flags & ARITHMETIC_UNSIGNED) {
      e.xor(e.dx, e.dx);
      e.div(i.src2.reg);
    } else {
      // Set dx to sign bit of src1 (dx:ax = dx:ax / src).
      e.mov(e.dx, e.ax);
      e.sar(e.dx, 15);
      e.idiv(i.src2.reg);
    }
    e.mov(i.dest.reg, e.ax);
  }
};
EMITTER(DIV_I32, MATCH(I<OPCODE_DIV, I32<>, I32<>, I32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // NOTE: RDX clobbered.
    e.mov(e.eax, i.src1.reg);
    if (i.instr->flags & ARITHMETIC_UNSIGNED) {
      e.xor(e.edx, e.edx);
      e.div(i.src2.reg);
    } else {
      // Set edx to sign bit of src1 (edx:eax = edx:eax / src).
      e.mov(e.edx, e.eax);
      e.sar(e.edx, 31);
      e.idiv(i.src2.reg);
    }
    e.mov(i.dest.reg, e.eax);
  }
};
EMITTER(DIV_I64, MATCH(I<OPCODE_DIV, I64<>, I64<>, I64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // NOTE: RDX clobbered.
    e.mov(e.rax, i.src1.reg);
    if (i.instr->flags & ARITHMETIC_UNSIGNED) {
      e.xor(e.rdx, e.rdx);
      e.div(i.src2.reg);
    } else {
      // Set rdx to sign bit of src1 (rdx:rax = rdx:rax / src).
      e.mov(e.rdx, e.rax);
      e.sar(e.rdx, 63);
      e.idiv(i.src2.reg);
    }
    e.mov(i.dest.reg, e.rax);
  }
};
EMITTER(DIV_F32, MATCH(I<OPCODE_DIV, F32<>, F32<>, F32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    XEASSERT(!i.instr->flags);
    e.vdivss(i.dest.reg, i.src1.reg, i.src2.reg);
  }
};
EMITTER(DIV_F64, MATCH(I<OPCODE_DIV, F64<>, F64<>, F64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    XEASSERT(!i.instr->flags);
    e.vdivsd(i.dest.reg, i.src1.reg, i.src2.reg);
  }
};
EMITTER(DIV_V128, MATCH(I<OPCODE_DIV, V128<>, V128<>, V128<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    XEASSERT(!i.instr->flags);
    e.vdivps(i.dest.reg, i.src1.reg, i.src2.reg);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_DIV,
    DIV_I8::Select,
    DIV_I16::Select,
    DIV_I32::Select,
    DIV_I64::Select,
    DIV_F32::Select,
    DIV_F64::Select,
    DIV_V128::Select);


// ============================================================================
// OPCODE_MUL_ADD
// ============================================================================
EMITTER(MUL_ADD_F32, MATCH(I<OPCODE_MUL_ADD, F32<>, F32<>, F32<>, F32<>>)) {
  // d = 1 * 2 + 3
  // $0 = $1×$0 + $2
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    if (i.dest.reg.getIdx() != i.src1.reg.getIdx()) {
      e.vmovss(i.dest.reg, i.src1.reg);
    }
    e.vfmadd213ss(i.dest.reg, i.src2.reg, i.src3.reg);
  }
};
EMITTER(MUL_ADD_F64, MATCH(I<OPCODE_MUL_ADD, F64<>, F64<>, F64<>, F64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    if (i.dest.reg.getIdx() != i.src1.reg.getIdx()) {
      e.vmovsd(i.dest.reg, i.src1.reg);
    }
    e.vfmadd213sd(i.dest.reg, i.src2.reg, i.src3.reg);
  }
};
EMITTER(MUL_ADD_V128, MATCH(I<OPCODE_MUL_ADD, V128<>, V128<>, V128<>, V128<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    if (i.dest.reg.getIdx() != i.src1.reg.getIdx()) {
      e.vmovdqa(i.dest.reg, i.src1.reg);
    }
    e.vfmadd213ps(i.dest.reg, i.src2.reg, i.src3.reg);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_MUL_ADD,
    MUL_ADD_F32::Select,
    MUL_ADD_F64::Select,
    MUL_ADD_V128::Select);


// ============================================================================
// OPCODE_MUL_SUB
// ============================================================================
EMITTER(MUL_SUB_F32, MATCH(I<OPCODE_MUL_SUB, F32<>, F32<>, F32<>, F32<>>)) {
  // d = 1 * 2 + 3
  // $0 = $1×$0 + $2
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    if (i.dest.reg.getIdx() != i.src1.reg.getIdx()) {
      e.vmovss(i.dest.reg, i.src1.reg);
    }
    e.vfmsub213ss(i.dest.reg, i.src2.reg, i.src3.reg);
  }
};
EMITTER(MUL_SUB_F64, MATCH(I<OPCODE_MUL_SUB, F64<>, F64<>, F64<>, F64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    if (i.dest.reg.getIdx() != i.src1.reg.getIdx()) {
      e.vmovsd(i.dest.reg, i.src1.reg);
    }
    e.vfmsub213sd(i.dest.reg, i.src2.reg, i.src3.reg);
  }
};
EMITTER(MUL_SUB_V128, MATCH(I<OPCODE_MUL_SUB, V128<>, V128<>, V128<>, V128<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    if (i.dest.reg.getIdx() != i.src1.reg.getIdx()) {
      e.vmovdqa(i.dest.reg, i.src1.reg);
    }
    e.vfmsub213ps(i.dest.reg, i.src2.reg, i.src3.reg);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_MUL_SUB,
    MUL_SUB_F32::Select,
    MUL_SUB_F64::Select,
    MUL_SUB_V128::Select);


// ============================================================================
// OPCODE_NEG
// ============================================================================
EMITTER(NEG_I8, MATCH(I<OPCODE_NEG, I8<>, I8<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    if (i.dest.reg.getIdx() != i.src1.reg.getIdx()) {
      // TODO(benvanik): remove mov.
      e.mov(i.dest.reg, i.src1.reg);
    }
    e.neg(i.dest.reg);
  }
};
EMITTER(NEG_I16, MATCH(I<OPCODE_NEG, I16<>, I16<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    if (i.dest.reg.getIdx() != i.src1.reg.getIdx()) {
      // TODO(benvanik): remove mov.
      e.mov(i.dest.reg, i.src1.reg);
    }
    e.neg(i.dest.reg);
  }
};
EMITTER(NEG_I32, MATCH(I<OPCODE_NEG, I32<>, I32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    if (i.dest.reg.getIdx() != i.src1.reg.getIdx()) {
      // TODO(benvanik): remove mov.
      e.mov(i.dest.reg, i.src1.reg);
    }
    e.neg(i.dest.reg);
  }
};
EMITTER(NEG_I64, MATCH(I<OPCODE_NEG, I64<>, I64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    if (i.dest.reg.getIdx() != i.src1.reg.getIdx()) {
      // TODO(benvanik): remove mov.
      e.mov(i.dest.reg, i.src1.reg);
    }
    e.neg(i.dest.reg);
  }
};
EMITTER(NEG_F32, MATCH(I<OPCODE_NEG, F32<>, F32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vpxor(i.dest.reg, i.src1.reg, e.GetXmmPtr(XMMSignMaskPS));
  }
};
EMITTER(NEG_F64, MATCH(I<OPCODE_NEG, F64<>, F64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vpxor(i.dest.reg, i.src1.reg, e.GetXmmPtr(XMMSignMaskPD));
  }
};
EMITTER(NEG_V128, MATCH(I<OPCODE_NEG, V128<>, V128<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    XEASSERT(!i.instr->flags);
    e.vpxor(i.dest.reg, i.src1.reg, e.GetXmmPtr(XMMSignMaskPS));
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_NEG,
    NEG_I8::Select,
    NEG_I16::Select,
    NEG_I32::Select,
    NEG_I64::Select,
    NEG_F32::Select,
    NEG_F64::Select,
    NEG_V128::Select);


// ============================================================================
// OPCODE_ABS
// ============================================================================
EMITTER(ABS_F32, MATCH(I<OPCODE_ABS, F32<>, F32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vpandn(i.dest.reg, i.src1.reg, e.GetXmmPtr(XMMSignMaskPS));
  }
};
EMITTER(ABS_F64, MATCH(I<OPCODE_ABS, F64<>, F64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vpandn(i.dest.reg, i.src1.reg, e.GetXmmPtr(XMMSignMaskPD));
  }
};
EMITTER(ABS_V128, MATCH(I<OPCODE_ABS, V128<>, V128<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    XEASSERT(!i.instr->flags);
    e.vpandn(i.dest.reg, i.src1.reg, e.GetXmmPtr(XMMSignMaskPS));
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_ABS,
    ABS_F32::Select,
    ABS_F64::Select,
    ABS_V128::Select);


// ============================================================================
// OPCODE_SQRT
// ============================================================================
EMITTER(SQRT_F32, MATCH(I<OPCODE_SQRT, F32<>, F32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vsqrtss(i.dest.reg, i.src1.reg);
  }
};
EMITTER(SQRT_F64, MATCH(I<OPCODE_SQRT, F64<>, F64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vsqrtsd(i.dest.reg, i.src1.reg);
  }
};
EMITTER(SQRT_V128, MATCH(I<OPCODE_SQRT, V128<>, V128<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vsqrtps(i.dest.reg, i.src1.reg);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_SQRT,
    SQRT_F32::Select,
    SQRT_F64::Select,
    SQRT_V128::Select);


// ============================================================================
// OPCODE_RSQRT
// ============================================================================
EMITTER(RSQRT_F32, MATCH(I<OPCODE_RSQRT, F32<>, F32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vrsqrtss(i.dest.reg, i.src1.reg);
  }
};
EMITTER(RSQRT_F64, MATCH(I<OPCODE_RSQRT, F64<>, F64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vcvtsd2ss(i.dest.reg, i.src1.reg);
    e.vrsqrtss(i.dest.reg, i.dest.reg);
    e.vcvtss2sd(i.dest.reg, i.dest.reg);
  }
};
EMITTER(RSQRT_V128, MATCH(I<OPCODE_RSQRT, V128<>, V128<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vrsqrtps(i.dest.reg, i.src1.reg);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_RSQRT,
    RSQRT_F32::Select,
    RSQRT_F64::Select,
    RSQRT_V128::Select);


// ============================================================================
// OPCODE_POW2
// ============================================================================


// ============================================================================
// OPCODE_LOG2
// ============================================================================


// ============================================================================
// OPCODE_DOT_PRODUCT_3
// ============================================================================
EMITTER(DOT_PRODUCT_3_V128, MATCH(I<OPCODE_DOT_PRODUCT_3, V128<>, V128<>, V128<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // http://msdn.microsoft.com/en-us/library/bb514054(v=vs.90).aspx
    // TODO(benvanik): verify ordering
    // TODO(benvanik): apparently this is very slow - find alternative?
    e.vdpps(i.dest.reg, i.src1.reg, i.src2.reg, B01110001);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_DOT_PRODUCT_3,
    DOT_PRODUCT_3_V128::Select);


// ============================================================================
// OPCODE_DOT_PRODUCT_4
// ============================================================================
EMITTER(DOT_PRODUCT_4_V128, MATCH(I<OPCODE_DOT_PRODUCT_4, V128<>, V128<>, V128<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // http://msdn.microsoft.com/en-us/library/bb514054(v=vs.90).aspx
    // TODO(benvanik): verify ordering
    // TODO(benvanik): apparently this is very slow - find alternative?
    e.vdpps(i.dest.reg, i.src1.reg, i.src2.reg, B11110001);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_DOT_PRODUCT_4,
    DOT_PRODUCT_4_V128::Select);


// ============================================================================
// OPCODE_AND
// ============================================================================
template <typename REG>
void AND_XX(X64Emitter& e, const REG& dest, const REG& src1, const REG& src2) {
  if (dest.getIdx() == src1.getIdx()) {
    e.and(dest, src2);
  } else if (dest.getIdx() == src2.getIdx()) {
    e.and(dest, src1);
  } else {
    e.mov(dest, src1);
    e.and(dest, src2);
  }
}
EMITTER(AND_I8, MATCH(I<OPCODE_AND, I8<>, I8<>, I8<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // TODO(benvanik): put dest/src1|2 together.
    AND_XX(e, i.dest.reg, i.src1.reg, i.src2.reg);
  }
};
EMITTER(AND_I16, MATCH(I<OPCODE_AND, I16<>, I16<>, I16<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // TODO(benvanik): put dest/src1|2 together.
    AND_XX(e, i.dest.reg, i.src1.reg, i.src2.reg);
  }
};
EMITTER(AND_I32, MATCH(I<OPCODE_AND, I32<>, I32<>, I32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // TODO(benvanik): put dest/src1|2 together.
    AND_XX(e, i.dest.reg, i.src1.reg, i.src2.reg);
  }
};
EMITTER(AND_I64, MATCH(I<OPCODE_AND, I64<>, I64<>, I64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // TODO(benvanik): put dest/src1|2 together.
    AND_XX(e, i.dest.reg, i.src1.reg, i.src2.reg);
  }
};
EMITTER(AND_V128, MATCH(I<OPCODE_AND, V128<>, V128<>, V128<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vpand(i.dest.reg, i.src1.reg, i.src2.reg);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_AND,
    AND_I8::Select,
    AND_I16::Select,
    AND_I32::Select,
    AND_I64::Select,
    AND_V128::Select);


// ============================================================================
// OPCODE_OR
// ============================================================================
template <typename REG>
void OR_XX(X64Emitter& e, const REG& dest, const REG& src1, const REG& src2) {
  if (dest.getIdx() == src1.getIdx()) {
    e.or(dest, src2);
  } else if (dest.getIdx() == src2.getIdx()) {
    e.or(dest, src1);
  } else {
    e.mov(dest, src1);
    e.or(dest, src2);
  }
}
EMITTER(OR_I8, MATCH(I<OPCODE_OR, I8<>, I8<>, I8<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // TODO(benvanik): put dest/src1|2 together.
    OR_XX(e, i.dest.reg, i.src1.reg, i.src2.reg);
  }
};
EMITTER(OR_I16, MATCH(I<OPCODE_OR, I16<>, I16<>, I16<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // TODO(benvanik): put dest/src1|2 together.
    OR_XX(e, i.dest.reg, i.src1.reg, i.src2.reg);
  }
};
EMITTER(OR_I32, MATCH(I<OPCODE_OR, I32<>, I32<>, I32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // TODO(benvanik): put dest/src1|2 together.
    OR_XX(e, i.dest.reg, i.src1.reg, i.src2.reg);
  }
};
EMITTER(OR_I64, MATCH(I<OPCODE_OR, I64<>, I64<>, I64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // TODO(benvanik): put dest/src1|2 together.
    OR_XX(e, i.dest.reg, i.src1.reg, i.src2.reg);
  }
};
EMITTER(OR_V128, MATCH(I<OPCODE_OR, V128<>, V128<>, V128<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vpor(i.dest.reg, i.src1.reg, i.src2.reg);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_OR,
    OR_I8::Select,
    OR_I16::Select,
    OR_I32::Select,
    OR_I64::Select,
    OR_V128::Select);


// ============================================================================
// OPCODE_XOR
// ============================================================================
template <typename REG>
void XOR_XX(X64Emitter& e, const REG& dest, const REG& src1, const REG& src2) {
  if (dest.getIdx() == src1.getIdx()) {
    e.xor(dest, src2);
  } else if (dest.getIdx() == src2.getIdx()) {
    e.xor(dest, src1);
  } else {
    e.mov(dest, src1);
    e.xor(dest, src2);
  }
}
EMITTER(XOR_I8, MATCH(I<OPCODE_XOR, I8<>, I8<>, I8<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // TODO(benvanik): put dest/src1|2 together.
    XOR_XX(e, i.dest.reg, i.src1.reg, i.src2.reg);
  }
};
EMITTER(XOR_I16, MATCH(I<OPCODE_XOR, I16<>, I16<>, I16<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // TODO(benvanik): put dest/src1|2 together.
    XOR_XX(e, i.dest.reg, i.src1.reg, i.src2.reg);
  }
};
EMITTER(XOR_I32, MATCH(I<OPCODE_XOR, I32<>, I32<>, I32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // TODO(benvanik): put dest/src1|2 together.
    XOR_XX(e, i.dest.reg, i.src1.reg, i.src2.reg);
  }
};
EMITTER(XOR_I64, MATCH(I<OPCODE_XOR, I64<>, I64<>, I64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // TODO(benvanik): put dest/src1|2 together.
    XOR_XX(e, i.dest.reg, i.src1.reg, i.src2.reg);
  }
};
EMITTER(XOR_V128, MATCH(I<OPCODE_XOR, V128<>, V128<>, V128<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.vpxor(i.dest.reg, i.src1.reg, i.src2.reg);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_XOR,
    XOR_I8::Select,
    XOR_I16::Select,
    XOR_I32::Select,
    XOR_I64::Select,
    XOR_V128::Select);


// ============================================================================
// OPCODE_NOT
// ============================================================================
template <typename REG>
void NOT_XX(X64Emitter& e, const REG& dest, const REG& src1) {
  if (dest.getIdx() == src1.getIdx()) {
    e.not(dest);
  } else {
    e.mov(dest, src1);
    e.not(dest);
  }
}
EMITTER(NOT_I8, MATCH(I<OPCODE_NOT, I8<>, I8<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // TODO(benvanik): put dest/src1 together.
    NOT_XX(e, i.dest.reg, i.src1.reg);
  }
};
EMITTER(NOT_I16, MATCH(I<OPCODE_NOT, I16<>, I16<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // TODO(benvanik): put dest/src1 together.
    NOT_XX(e, i.dest.reg, i.src1.reg);
  }
};
EMITTER(NOT_I32, MATCH(I<OPCODE_NOT, I32<>, I32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // TODO(benvanik): put dest/src1 together.
    NOT_XX(e, i.dest.reg, i.src1.reg);
  }
};
EMITTER(NOT_I64, MATCH(I<OPCODE_NOT, I64<>, I64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // TODO(benvanik): put dest/src1 together.
    NOT_XX(e, i.dest.reg, i.src1.reg);
  }
};
EMITTER(NOT_V128, MATCH(I<OPCODE_NOT, V128<>, V128<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // dest = src ^ 0xFFFF...
    e.vpxor(i.dest.reg, i.src1.reg, e.GetXmmPtr(XMMOne));
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_NOT,
    NOT_I8::Select,
    NOT_I16::Select,
    NOT_I32::Select,
    NOT_I64::Select,
    NOT_V128::Select);


// ============================================================================
// OPCODE_SHL
// ============================================================================
EMITTER(SHL_I8, MATCH(I<OPCODE_SHL, I8<>, I8<>, I8<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.shlx(i.dest.reg.cvt32(), i.src1.reg.cvt32(), i.src2.reg.cvt32());
  }
};
EMITTER(SHL_I16, MATCH(I<OPCODE_SHL, I16<>, I16<>, I8<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.shlx(i.dest.reg.cvt32(), i.src1.reg.cvt32(), i.src2.reg.cvt32());
  }
};
EMITTER(SHL_I32, MATCH(I<OPCODE_SHL, I32<>, I32<>, I8<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.shlx(i.dest.reg, i.src1.reg, i.src2.reg.cvt32());
  }
};
EMITTER(SHL_I64, MATCH(I<OPCODE_SHL, I64<>, I64<>, I8<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.shlx(i.dest.reg, i.src1.reg, i.src2.reg.cvt32());
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_SHL,
    SHL_I8::Select,
    SHL_I16::Select,
    SHL_I32::Select,
    SHL_I64::Select);


// ============================================================================
// OPCODE_SHR
// ============================================================================
EMITTER(SHR_I8, MATCH(I<OPCODE_SHR, I8<>, I8<>, I8<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.shrx(i.dest.reg.cvt32(), i.src1.reg.cvt32(), i.src2.reg.cvt32());
  }
};
EMITTER(SHR_I16, MATCH(I<OPCODE_SHR, I16<>, I16<>, I8<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.shrx(i.dest.reg.cvt32(), i.src1.reg.cvt32(), i.src2.reg.cvt32());
  }
};
EMITTER(SHR_I32, MATCH(I<OPCODE_SHR, I32<>, I32<>, I8<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.shrx(i.dest.reg, i.src1.reg, i.src2.reg.cvt32());
  }
};
EMITTER(SHR_I64, MATCH(I<OPCODE_SHR, I64<>, I64<>, I8<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.shrx(i.dest.reg, i.src1.reg, i.src2.reg.cvt32());
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_SHR,
    SHR_I8::Select,
    SHR_I16::Select,
    SHR_I32::Select,
    SHR_I64::Select);


// ============================================================================
// OPCODE_SHA
// ============================================================================
EMITTER(SHA_I8, MATCH(I<OPCODE_SHA, I8<>, I8<>, I8<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.sarx(i.dest.reg.cvt32(), i.src1.reg.cvt32(), i.src2.reg.cvt32());
  }
};
EMITTER(SHA_I16, MATCH(I<OPCODE_SHA, I16<>, I16<>, I8<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.sarx(i.dest.reg.cvt32(), i.src1.reg.cvt32(), i.src2.reg.cvt32());
  }
};
EMITTER(SHA_I32, MATCH(I<OPCODE_SHA, I32<>, I32<>, I8<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.sarx(i.dest.reg, i.src1.reg, i.src2.reg.cvt32());
  }
};
EMITTER(SHA_I64, MATCH(I<OPCODE_SHA, I64<>, I64<>, I8<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.sarx(i.dest.reg, i.src1.reg, i.src2.reg.cvt32());
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_SHA,
    SHA_I8::Select,
    SHA_I16::Select,
    SHA_I32::Select,
    SHA_I64::Select);


// ============================================================================
// OPCODE_VECTOR_SHL
// ============================================================================
EMITTER(VECTOR_SHL_V128, MATCH(I<OPCODE_VECTOR_SHL, V128<>, V128<>, V128<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // src shift mask may have values >31, and x86 sets to zero when
    // that happens so we mask.
    e.vandps(e.xmm0, i.src2.reg, e.GetXmmPtr(XMMShiftMaskPS));
    e.vpsllvd(i.dest.reg, i.src1.reg, e.xmm0);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_VECTOR_SHL,
    VECTOR_SHL_V128::Select);


// ============================================================================
// OPCODE_VECTOR_SHR
// ============================================================================
EMITTER(VECTOR_SHR_V128, MATCH(I<OPCODE_VECTOR_SHR, V128<>, V128<>, V128<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // src shift mask may have values >31, and x86 sets to zero when
    // that happens so we mask.
    e.vandps(e.xmm0, i.src2.reg, e.GetXmmPtr(XMMShiftMaskPS));
    e.vpsrlvd(i.dest.reg, i.src1.reg, e.xmm0);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_VECTOR_SHR,
    VECTOR_SHR_V128::Select);


// ============================================================================
// OPCODE_VECTOR_SHA
// ============================================================================
EMITTER(VECTOR_SHA_V128, MATCH(I<OPCODE_VECTOR_SHA, V128<>, V128<>, V128<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // src shift mask may have values >31, and x86 sets to zero when
    // that happens so we mask.
    e.vandps(e.xmm0, i.src2.reg, e.GetXmmPtr(XMMShiftMaskPS));
    e.vpsravd(i.dest.reg, i.src1.reg, e.xmm0);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_VECTOR_SHA,
    VECTOR_SHA_V128::Select);


// ============================================================================
// OPCODE_ROTATE_LEFT
// ============================================================================
template <typename REG>
void ROTATE_LEFT_XX(X64Emitter& e, const REG& dest, const REG& src1, const Reg8& shamt) {
  if (shamt.getIdx() != e.cl.getIdx()) {
    e.mov(e.cl, shamt);
  }
  if (dest.getIdx() == src1.getIdx()) {
    e.rol(dest, e.cl);
  } else {
    e.mov(dest, src1);
    e.rol(dest, e.cl);
  }
}
EMITTER(ROTATE_LEFT_I8, MATCH(I<OPCODE_ROTATE_LEFT, I8<>, I8<>, I8<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // TODO(benvanik): put dest/src1 together, src2 in cl.
    ROTATE_LEFT_XX(e, i.dest.reg, i.src1.reg, i.src2.reg);
  }
};
EMITTER(ROTATE_LEFT_I16, MATCH(I<OPCODE_ROTATE_LEFT, I16<>, I16<>, I8<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // TODO(benvanik): put dest/src1 together, src2 in cl.
    ROTATE_LEFT_XX(e, i.dest.reg, i.src1.reg, i.src2.reg);
  }
};
EMITTER(ROTATE_LEFT_I32, MATCH(I<OPCODE_ROTATE_LEFT, I32<>, I32<>, I8<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // TODO(benvanik): put dest/src1 together, src2 in cl.
    ROTATE_LEFT_XX(e, i.dest.reg, i.src1.reg, i.src2.reg);
  }
};
EMITTER(ROTATE_LEFT_I64, MATCH(I<OPCODE_ROTATE_LEFT, I64<>, I64<>, I8<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // TODO(benvanik): put dest/src1 together, src2 in cl.
    ROTATE_LEFT_XX(e, i.dest.reg, i.src1.reg, i.src2.reg);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_ROTATE_LEFT,
    ROTATE_LEFT_I8::Select,
    ROTATE_LEFT_I16::Select,
    ROTATE_LEFT_I32::Select,
    ROTATE_LEFT_I64::Select);


// ============================================================================
// OPCODE_BYTE_SWAP
// ============================================================================
EMITTER(BYTE_SWAP_I16, MATCH(I<OPCODE_BYTE_SWAP, I16<>, I16<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // TODO(benvanik): put src1 in dest.
    if (i.dest.reg.getIdx() != i.src1.reg.getIdx()) {
      e.mov(i.dest.reg, i.src1.reg);
    }
    e.ror(i.dest.reg, 8);
  }
};
EMITTER(BYTE_SWAP_I32, MATCH(I<OPCODE_BYTE_SWAP, I32<>, I32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // TODO(benvanik): put src1 in dest.
    if (i.dest.reg.getIdx() != i.src1.reg.getIdx()) {
      e.mov(i.dest.reg, i.src1.reg);
    }
    e.bswap(i.dest.reg);
  }
};
EMITTER(BYTE_SWAP_I64, MATCH(I<OPCODE_BYTE_SWAP, I64<>, I64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // TODO(benvanik): put src1 in dest.
    if (i.dest.reg.getIdx() != i.src1.reg.getIdx()) {
      e.mov(i.dest.reg, i.src1.reg);
    }
    e.bswap(i.dest.reg);
  }
};
EMITTER(BYTE_SWAP_V128, MATCH(I<OPCODE_BYTE_SWAP, V128<>, V128<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // TODO(benvanik): find a way to do this without the memory load.
    e.vpshufb(i.dest.reg, i.src1.reg, e.GetXmmPtr(XMMByteSwapMask));
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_BYTE_SWAP,
    BYTE_SWAP_I16::Select,
    BYTE_SWAP_I32::Select,
    BYTE_SWAP_I64::Select,
    BYTE_SWAP_V128::Select);


// ============================================================================
// OPCODE_CNTLZ
// ============================================================================
EMITTER(CNTLZ_I8, MATCH(I<OPCODE_CNTLZ, I8<>, I8<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    // No 8bit lzcnt, so do 16 and sub 8.
    e.movzx(i.dest.reg.cvt16(), i.src1.reg);
    e.lzcnt(i.dest.reg.cvt16(), i.dest.reg.cvt16());
    e.sub(i.dest.reg, 8);
  }
};
EMITTER(CNTLZ_I16, MATCH(I<OPCODE_CNTLZ, I8<>, I16<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.lzcnt(i.dest.reg.cvt32(), i.src1.reg);
  }
};
EMITTER(CNTLZ_I32, MATCH(I<OPCODE_CNTLZ, I8<>, I32<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.lzcnt(i.dest.reg.cvt32(), i.src1.reg);
  }
};
EMITTER(CNTLZ_I64, MATCH(I<OPCODE_CNTLZ, I8<>, I64<>>)) {
  static void Emit(X64Emitter& e, const EmitArgType& i) {
    e.lzcnt(i.dest.reg.cvt32(), i.src1.reg);
  }
};
EMITTER_OPCODE_TABLE(
    OPCODE_CNTLZ,
    CNTLZ_I8::Select,
    CNTLZ_I16::Select,
    CNTLZ_I32::Select,
    CNTLZ_I64::Select);


// OPCODE_INSERT
// OPCODE_EXTRACT
// OPCODE_SPLAT
// OPCODE_PERMUTE
// OPCODE_SWIZZLE
// OPCODE_PACK
// OPCODE_UNPACK
// OPCODE_COMPARE_EXCHANGE
// OPCODE_ATOMIC_EXCHANGE
// OPCODE_ATOMIC_ADD
// OPCODE_ATOMIC_SUB


// TODO(benvanik): sequence extract/splat:
//  v0.i32 = extract v0.v128, 0
//  v0.v128 = splat v0.i32
// This can be a single broadcast.


namespace {
static SequenceSelectFn sequence_table[__OPCODE_MAX_VALUE] = { 0 };
}  // namespace

void alloy::backend::x64::RegisterSequences() {
#define REGISTER_EMITTER_OPCODE_TABLE(opcode) \
  sequence_table[opcode] = &Select##opcode;
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_COMMENT);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_NOP);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_SOURCE_OFFSET);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_DEBUG_BREAK);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_DEBUG_BREAK_TRUE);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_TRAP);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_TRAP_TRUE);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_CALL);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_CALL_TRUE);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_CALL_INDIRECT);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_CALL_INDIRECT_TRUE);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_CALL_EXTERN);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_RETURN);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_RETURN_TRUE);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_SET_RETURN_ADDRESS);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_BRANCH);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_BRANCH_TRUE);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_BRANCH_FALSE);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_ASSIGN);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_CAST);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_ZERO_EXTEND);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_SIGN_EXTEND);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_TRUNCATE);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_CONVERT);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_ROUND);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_VECTOR_CONVERT_I2F);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_VECTOR_CONVERT_F2I);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_LOAD_VECTOR_SHL);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_LOAD_VECTOR_SHR);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_LOAD_CLOCK);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_LOAD_LOCAL);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_STORE_LOCAL);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_LOAD_CONTEXT);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_STORE_CONTEXT);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_LOAD);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_STORE);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_PREFETCH);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_MAX);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_MIN);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_SELECT);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_IS_TRUE);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_IS_FALSE);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_COMPARE_EQ);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_COMPARE_NE);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_COMPARE_SLT);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_COMPARE_SLE);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_COMPARE_SGT);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_COMPARE_SGE);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_COMPARE_ULT);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_COMPARE_ULE);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_COMPARE_UGT);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_COMPARE_UGE);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_DID_CARRY);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_DID_OVERFLOW);
  //REGISTER_EMITTER_OPCODE_TABLE(OPCODE_DID_SATURATE);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_VECTOR_COMPARE_EQ);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_VECTOR_COMPARE_SGT);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_VECTOR_COMPARE_SGE);
  //REGISTER_EMITTER_OPCODE_TABLE(OPCODE_VECTOR_COMPARE_UGT);
  //REGISTER_EMITTER_OPCODE_TABLE(OPCODE_VECTOR_COMPARE_UGE);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_ADD);
  //REGISTER_EMITTER_OPCODE_TABLE(OPCODE_ADD_CARRY);
  //REGISTER_EMITTER_OPCODE_TABLE(OPCODE_VECTOR_ADD);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_SUB);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_MUL);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_MUL_HI);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_DIV);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_MUL_ADD);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_MUL_SUB);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_NEG);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_ABS);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_SQRT);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_RSQRT);
  //REGISTER_EMITTER_OPCODE_TABLE(OPCODE_POW2);
  //REGISTER_EMITTER_OPCODE_TABLE(OPCODE_LOG2);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_DOT_PRODUCT_3);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_DOT_PRODUCT_4);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_AND);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_OR);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_XOR);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_NOT);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_SHL);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_SHR);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_SHA);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_VECTOR_SHL);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_VECTOR_SHR);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_VECTOR_SHA);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_ROTATE_LEFT);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_BYTE_SWAP);
  REGISTER_EMITTER_OPCODE_TABLE(OPCODE_CNTLZ);
  //REGISTER_EMITTER_OPCODE_TABLE(OPCODE_INSERT);
  //REGISTER_EMITTER_OPCODE_TABLE(OPCODE_EXTRACT);
  //REGISTER_EMITTER_OPCODE_TABLE(OPCODE_SPLAT);
  //REGISTER_EMITTER_OPCODE_TABLE(OPCODE_PERMUTE);
  //REGISTER_EMITTER_OPCODE_TABLE(OPCODE_SWIZZLE);
  //REGISTER_EMITTER_OPCODE_TABLE(OPCODE_PACK);
  //REGISTER_EMITTER_OPCODE_TABLE(OPCODE_UNPACK);
  //REGISTER_EMITTER_OPCODE_TABLE(OPCODE_COMPARE_EXCHANGE);
  //REGISTER_EMITTER_OPCODE_TABLE(OPCODE_ATOMIC_EXCHANGE);
  //REGISTER_EMITTER_OPCODE_TABLE(OPCODE_ATOMIC_ADD);
  //REGISTER_EMITTER_OPCODE_TABLE(OPCODE_ATOMIC_SUB);
}

bool alloy::backend::x64::SelectSequence(X64Emitter& e, const Instr* i, const Instr** new_tail) {
  auto selector = sequence_table[i->opcode->num];
  if (selector && selector(e, i, new_tail)) {
    return true;
  }
  return false;
}
