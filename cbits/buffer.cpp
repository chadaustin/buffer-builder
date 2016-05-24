#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <assert.h>

#include "branchlut.h"

/*
 * Append utf8, ASCII7, raw bytes, utf16
 * JSON encode utf8, ascii7, utf16
 */

#if defined(__clang__) || defined(__GNUC__)
  #define BW_NOINLINE __attribute__((noinline))
#else
  #define BW_NOINLINE
#endif

#if defined(__clang__) || defined(__GNUC__)
  #define BW_LIKELY(x) __builtin_expect(!!(x), 1)
  #define BW_UNLIKELY(x) __builtin_expect(!!(x), 0)
#else
  #define BW_LIKELY(x) x
  #define BW_UNLIKELY(x) x
#endif

#define BW_ENSURE_CAPACITY(bw, cap, count_size)     \
    do {                                            \
        if (!ensureCapacity((bw), (cap))) {         \
            if (bw->counting_size) {                \
                bw->counted_size += count_size;     \
            }                                       \
            return;                                 \
        }                                           \
    } while (0)


namespace {
    static const unsigned char DEC2HEX[16] = {
        '0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f'
    };

    struct BufferWriter {
        unsigned char* end; // the most commonly-accessed field
        unsigned char* capacity;
        unsigned char* data;

        bool counting_size;
        size_t counted_size;

        // invariants:
        // if (counting_size), end, capacity, and data are null
        // else, counted_size is zero and:
        // if (end <= capacity), then data is non-null and end >= data
        // if (capacity == 0), then data and end are null, and object is dead

        // Note: We mark the object dead upon growth failure in order
        // to avoid the need to check for errors after every single
        // append function.  The error can be raised upon the final
        // pointer release.
    };

    BW_NOINLINE bool actuallyGrow(BufferWriter* bw, size_t amount) {
        if (bw->capacity == 0) {
            // object is dead, ignore append request
            return false;
        }

        size_t size = bw->end - bw->data;
        size_t newCapacity = bw->capacity - bw->data;
        do {
            // TODO: detect overflow?
            newCapacity *= 2;
        } while (newCapacity < size + amount);

        unsigned char* newData = reinterpret_cast<unsigned char*>(realloc(bw->data, newCapacity));
        if (newData) {
            bw->data = newData;
            bw->end = newData + size;
            bw->capacity = newData + newCapacity;
            return true;
        } else {
            // TODO: try smaller growth?
            free(bw->data);
            bw->data = 0;
            bw->end = 0;
            bw->capacity = 0;
            return false;
        }
    }

    inline bool ensureCapacity(BufferWriter* bw, size_t amount) {
        if (BW_LIKELY(bw->end + amount <= bw->capacity)) {
            return true;
        } else {
            return actuallyGrow(bw, amount);
        }
    }
}

extern "C" BufferWriter* bw_new(size_t initialCapacity) {
    if (!initialCapacity) {
        initialCapacity = 1;
    }

    BufferWriter* bw = reinterpret_cast<BufferWriter*>(malloc(sizeof(BufferWriter)));
    if (!bw) {
        return 0;
    }

    bw->data = reinterpret_cast<unsigned char*>(malloc(initialCapacity));
    if (!bw->data) {
        free(bw);
        return 0;
    }

    bw->end = bw->data;
    bw->capacity = bw->data + initialCapacity;
    bw->counting_size = false;
    bw->counted_size = 0;
    return bw;
}

extern "C" BufferWriter* bw_new_length_calculator() {
    BufferWriter* bw = reinterpret_cast<BufferWriter*>(malloc(sizeof(BufferWriter)));
    if (!bw) {
        return 0;
    }

    bw->data = 0;
    bw->end = 0;
    bw->capacity = 0;
    bw->counting_size = true;
    bw->counted_size = 0;
    return bw;
}

extern "C" void bw_free(BufferWriter* bw) {
    free(bw->data);
    free(bw);
}

extern "C" void bw_trim(BufferWriter* bw) {
    if (bw->data == 0) {
        return;
    }

    if (bw->end < bw->capacity) {
        // try to shrink
        size_t size = bw->end - bw->data;
        unsigned char* data = reinterpret_cast<unsigned char*>(realloc(bw->data, size));
        if (data) {
            bw->data = data;
            bw->end = data + size;
            bw->capacity = data + size;
        } else {
            // no problem
        }
    }

}

extern "C" size_t bw_get_size(BufferWriter* bw) {
    if (bw->counting_size) {
        return bw->counted_size;
    } else {
        return bw->end - bw->data;
    }
}

extern "C" unsigned char* bw_release_address(BufferWriter* bw) {
    unsigned char* data = bw->data;

    bw->end = 0;
    bw->capacity = 0;
    bw->data = 0;
    bw->counting_size = false;
    bw->counted_size = 0;
    return data; // null if dead
}

extern "C" void bw_append_byte(BufferWriter* bw, unsigned char byte) {
    BW_ENSURE_CAPACITY(bw, 1, 1);

    bw->end[0] = byte;
    bw->end += 1;
}

inline size_t utf8_char_length(uint32_t c) {
    if (c <= 0x7F) {
        return 1;
    } else if (c <= 0x7FF) {
        return 2;
    } else if (c <= 0xFFFF) {
        return 3;
    } else if (c <= 0x1FFFFF) {
        return 4;
    } else {
        // Unicode characters out of this range are illegal...
        return 0;
    }
}

extern "C" void bw_append_char_utf8(BufferWriter* bw, uint32_t c) {
    BW_ENSURE_CAPACITY(bw, 4, utf8_char_length(c));

    unsigned char* data = bw->end;

    if (c <= 0x7F) {
        data[0] = static_cast<unsigned char>(c);
        data += 1;
    } else if (c <= 0x7FF) {
        data[0] = static_cast<unsigned char>(0xC0 | (c >> 6));
        data[1] = static_cast<unsigned char>(0x80 | (c & 0x3F));
        data += 2;
    } else if (c <= 0xFFFF) {
        data[0] = static_cast<unsigned char>(0xE0 | (c >> 12));
        data[1] = static_cast<unsigned char>(0x80 | ((c >> 6) & 0x3F));
        data[2] = static_cast<unsigned char>(0x80 | (c & 0x3F));
        data += 3;
    } else if (c <= 0x1FFFFF) {
        data[0] = static_cast<unsigned char>(0xF0 | (c >> 18));
        data[1] = static_cast<unsigned char>(0x80 | ((c >> 12) & 0x3F));
        data[2] = static_cast<unsigned char>(0x80 | ((c >> 6) & 0x3F));
        data[3] = static_cast<unsigned char>(0x80 | (c & 0x3F));
        data += 4;
    } else {
        // Unicode characters out of this range are illegal...
        // should we assert?  or just ignore...
    }
    bw->end = data;
}

extern "C" void bw_append_bs(BufferWriter* bw, size_t size, const unsigned char* data) {
    BW_ENSURE_CAPACITY(bw, size, size);

    memcpy(bw->end, data, size);
    bw->end += size;
}

extern "C" void bw_append_bsz(BufferWriter* bw, const unsigned char* data) {
    bw_append_bs(bw, strlen(reinterpret_cast<const char*>(data)), data);
}

extern "C" void bw_append_byte7(BufferWriter* bw, unsigned char byte) {
    BW_ENSURE_CAPACITY(bw, 1, 1);

    bw->end[0] = byte & 0x7F;
    bw->end += 1;
}

extern "C" void bw_append_bs7(BufferWriter* bw, size_t size, const unsigned char* data) {
    BW_ENSURE_CAPACITY(bw, size, size);

    unsigned char* out = bw->end;
    for (size_t i = 0; i < size; ++i) {
        out[i] = data[i] & 0x7F;
    }
    bw->end += size;
}

extern "C" void bw_append_bsz7(BufferWriter* bw, const unsigned char* data) {
    bw_append_bs7(bw, strlen(reinterpret_cast<const char*>(data)), data);
}

inline size_t count_escaped_json_length(size_t size, const unsigned char* data) {
    size_t rv = 2;
    for (size_t i = 0; i < size; ++i) {
        switch (data[i]) {
            case '\n': rv += 2; break;
            case '\r': rv += 2; break;
            case '\t': rv += 2; break;
            case '\\': rv += 2; break;
            case '\"': rv += 2; break;
            default:
                if (data[i] <= 0x1F) {
                    rv += 6;
                } else {
                    rv += 1;
                }
                break;
        }
    }
    return rv;
}

extern "C" void bw_append_json_escaped(BufferWriter* bw, size_t size, const unsigned char* data) {
    BW_ENSURE_CAPACITY(bw, 2 + size * 2, count_escaped_json_length(size, data));

    unsigned char* dest = bw->end;
    *dest++ = '\"';

    size_t i = 0;
    while (i < size) {
        switch (data[i]) {
            case '\n': *dest++ = '\\'; *dest++ = 'n';  break;
            case '\r': *dest++ = '\\'; *dest++ = 'r';  break;
            case '\t': *dest++ = '\\'; *dest++ = 't';  break;
            case '\\': *dest++ = '\\'; *dest++ = '\\'; break;
            case '\"': *dest++ = '\\'; *dest++ = '\"'; break;
            default:
                unsigned char d = data[i];
                if (d <= 0x1F) {
                    *dest++ = '\\';
                    *dest++ = 'u';
                    *dest++ = '0';
                    *dest++ = '0';
                    *dest++ = DEC2HEX[d >> 4];
                    *dest++ = DEC2HEX[d & 0xF];
                } else {
                    *dest++ = d;
                }
                break;
        }
        ++i;
    }

    *dest++ = '\"';
    bw->end = dest;
}

inline size_t count_decimal_signed_int_length(int64_t i) {
    char buf[32]; // enough
    return i64toa_branchlut(i, buf);
}

extern "C" void bw_append_decimal_signed_int(BufferWriter* bw, int64_t i) {
    BW_ENSURE_CAPACITY(bw, 32, count_decimal_signed_int_length(i)); // enough

    unsigned used = i64toa_branchlut(i, reinterpret_cast<char*>(bw->end));
    bw->end += used;
}

inline size_t count_decimal_double_length(double d) {
    char buf[318];
    return sprintf(buf, "%f", d);
}

extern "C" void bw_append_decimal_double(BufferWriter* bw, double d) {
    BW_ENSURE_CAPACITY(bw, 318, count_decimal_double_length(d)); // sprintf("%f", -DBL_MAX);

    size_t used = sprintf(reinterpret_cast<char*>(bw->end), "%f", d);
    bw->end += used;
}

// From <https://github.com/bos/text/blob/f74427c954fb4479f9db5025f27775e29ace125f/cbits/cbits.c#L234>
extern "C" void bw_encode_utf8(uint8_t **destp, const uint16_t *src, size_t srcoff, size_t srclen) {
  const uint16_t *srcend;
  uint8_t *dest = *destp;

  src += srcoff;
  srcend = src + srclen;

 ascii:
#if defined(__x86_64__)
  while (srcend - src >= 4) {
    uint64_t w = *((uint64_t *) src);

    if (w & 0xFF80FF80FF80FF80ULL) {
      if (!(w & 0x000000000000FF80ULL)) {
        *dest++ = w & 0xFFFF;
        src++;
        if (!(w & 0x00000000FF800000ULL)) {
          *dest++ = (w >> 16) & 0xFFFF;
          src++;
          if (!(w & 0x0000FF8000000000ULL)) {
            *dest++ = (w >> 32) & 0xFFFF;
            src++;
          }
        }
      }
      break;
    }
    *dest++ = w & 0xFFFF;
    *dest++ = (w >> 16) & 0xFFFF;
    *dest++ = (w >> 32) & 0xFFFF;
    *dest++ = w >> 48;
    src += 4;
  }
#endif

#if defined(__i386__)
  while (srcend - src >= 2) {
    uint32_t w = *((uint32_t *) src);

    if (w & 0xFF80FF80)
      break;
    *dest++ = w & 0xFFFF;
    *dest++ = w >> 16;
    src += 2;
  }
#endif

  while (src < srcend) {
    uint16_t w = *src++;

    if (w <= 0x7F) {
      *dest++ = w;
      /* An ASCII byte is likely to begin a run of ASCII bytes.
     Falling back into the fast path really helps performance. */
      goto ascii;
    }
    else if (w <= 0x7FF) {
      *dest++ = (w >> 6) | 0xC0;
      *dest++ = (w & 0x3f) | 0x80;
    }
    else if (w < 0xD800 || w > 0xDBFF) {
      *dest++ = (w >> 12) | 0xE0;
      *dest++ = ((w >> 6) & 0x3F) | 0x80;
      *dest++ = (w & 0x3F) | 0x80;
    } else {
      uint32_t c = ((((uint32_t) w) - 0xD800) << 10) +
    (((uint32_t) *src++) - 0xDC00) + 0x10000;
      *dest++ = (c >> 18) | 0xF0;
      *dest++ = ((c >> 12) & 0x3F) | 0x80;
      *dest++ = ((c >> 6) & 0x3F) | 0x80;
      *dest++ = (c & 0x3F) | 0x80;
    }
  }

  *destp = dest;
}

extern "C" void bw_append_json_escaped_utf16(BufferWriter* bw, size_t size, unsigned short* words) {
    // TODO: handle malloc failure
    // ideally no temp buffer would be required here.
    unsigned char* utf8 = reinterpret_cast<unsigned char*>(malloc(size * 4));
    assert(utf8);
    unsigned char* utf8end = utf8;

    bw_encode_utf8(&utf8end, words, 0, size);

    bw_append_json_escaped(bw, utf8end - utf8, utf8);

    free(utf8);
}

static const unsigned char UNRESERVED[256] = {
    0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,
    0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,
    0,0,0,0, 0,0,0,0, 0,0,0,0, 0,1,1,0,
    1,1,1,1, 1,1,1,1, 1,1,0,0, 0,0,0,0,

    0,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1,
    1,1,1,1, 1,1,1,1, 1,1,1,0, 0,0,0,1,
    0,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1,
    1,1,1,1, 1,1,1,1, 1,1,1,0, 0,0,1,0,

    0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,
    0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,
    0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,
    0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,

    0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,
    0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,
    0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,
    0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0
};

inline size_t count_url_encoded_length(size_t size, const unsigned char* data) {
    size_t rv = 0;
    for (size_t i = 0; i < size; ++i) {
        if (UNRESERVED[data[i]]) {
            rv += 1;
        } else {
            rv += 3;
        }
    }
    return rv;
}

extern "C" void bw_append_url_encoded(BufferWriter* bw, size_t size, const unsigned char* data) {
    // worst case
    BW_ENSURE_CAPACITY(bw, size * 3, count_url_encoded_length(size, data));

    const unsigned char* src = data;
    unsigned char* dst = bw->end;
    while (size--) {
        unsigned char c = *src++;
        if (UNRESERVED[c]) {
            *dst++ = c;
        } else {
            dst[0] = '%';
            dst[1] = DEC2HEX[c >> 4];
            dst[2] = DEC2HEX[c & 0xF];
            dst += 3;
        }
    }

    bw->end = dst;
}
