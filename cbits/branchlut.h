/** <https://github.com/miloyip/itoa-benchmark/blob/940542a7770155ee3e9f2777ebc178dc899b43e0/src/branchlut.cpp>
 */

#pragma once

#include <stdint.h>

size_t u32toa_branchlut(uint32_t value, char* buffer);
size_t i32toa_branchlut(int32_t value, char* buffer);
size_t u64toa_branchlut(uint64_t value, char* buffer);
size_t i64toa_branchlut(int64_t value, char* buffer);
