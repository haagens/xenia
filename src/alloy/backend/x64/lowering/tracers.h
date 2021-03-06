/**
 ******************************************************************************
 * Xenia : Xbox 360 Emulator Research Project                                 *
 ******************************************************************************
 * Copyright 2014 Ben Vanik. All rights reserved.                             *
 * Released under the BSD license - see LICENSE in the root for more details. *
 ******************************************************************************
 */

#ifndef ALLOY_BACKEND_X64_X64_LOWERING_TRACERS_H_
#define ALLOY_BACKEND_X64_X64_LOWERING_TRACERS_H_

#include <alloy/core.h>

#if XE_LIKE_WIN32
#include <xmmintrin.h>
#else
typedef union __declspec(align(16)) __m128 {
	float m128_f32[4];
	uint64_t m128_u64[2];
	int8_t m128_i8[16];
	int16_t m128_i16[8];
	int32_t m128_i32[4];
	int64_t m128_i64[2];
	uint8_t m128_u8[16];
	uint16_t m128_u16[8];
	uint32_t m128_u32[4];
} __m128;
#endif


namespace alloy {
namespace backend {
namespace x64 {
class X64Emitter;
namespace lowering {

void TraceString(void* raw_context, const char* str);

void TraceContextLoadI8(void* raw_context, uint64_t offset, uint8_t value);
void TraceContextLoadI16(void* raw_context, uint64_t offset, uint16_t value);
void TraceContextLoadI32(void* raw_context, uint64_t offset, uint32_t value);
void TraceContextLoadI64(void* raw_context, uint64_t offset, uint64_t value);
void TraceContextLoadF32(void* raw_context, uint64_t offset, __m128 value);
void TraceContextLoadF64(void* raw_context, uint64_t offset, __m128 value);
void TraceContextLoadV128(void* raw_context, uint64_t offset, __m128 value);

void TraceContextStoreI8(void* raw_context, uint64_t offset, uint8_t value);
void TraceContextStoreI16(void* raw_context, uint64_t offset, uint16_t value);
void TraceContextStoreI32(void* raw_context, uint64_t offset, uint32_t value);
void TraceContextStoreI64(void* raw_context, uint64_t offset, uint64_t value);
void TraceContextStoreF32(void* raw_context, uint64_t offset, __m128 value);
void TraceContextStoreF64(void* raw_context, uint64_t offset, __m128 value);
void TraceContextStoreV128(void* raw_context, uint64_t offset, __m128 value);

void TraceMemoryLoadI8(void* raw_context, uint64_t address, uint8_t value);
void TraceMemoryLoadI16(void* raw_context, uint64_t address, uint16_t value);
void TraceMemoryLoadI32(void* raw_context, uint64_t address, uint32_t value);
void TraceMemoryLoadI64(void* raw_context, uint64_t address, uint64_t value);
void TraceMemoryLoadF32(void* raw_context, uint64_t address, __m128 value);
void TraceMemoryLoadF64(void* raw_context, uint64_t address, __m128 value);
void TraceMemoryLoadV128(void* raw_context, uint64_t address, __m128 value);

void TraceMemoryStoreI8(void* raw_context, uint64_t address, uint8_t value);
void TraceMemoryStoreI16(void* raw_context, uint64_t address, uint16_t value);
void TraceMemoryStoreI32(void* raw_context, uint64_t address, uint32_t value);
void TraceMemoryStoreI64(void* raw_context, uint64_t address, uint64_t value);
void TraceMemoryStoreF32(void* raw_context, uint64_t address, __m128 value);
void TraceMemoryStoreF64(void* raw_context, uint64_t address, __m128 value);
void TraceMemoryStoreV128(void* raw_context, uint64_t address, __m128 value);

}  // namespace lowering
}  // namespace x64
}  // namespace backend
}  // namespace alloy


#endif  // ALLOY_BACKEND_X64_X64_LOWERING_TRACERS_H_
