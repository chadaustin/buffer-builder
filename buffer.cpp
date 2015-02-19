#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

const size_t TRIM_THRESHOLD = 8192; // minimum number of bytes saved to trim

/*
 * Append utf8, ASCII7, raw bytes, utf16
 * JSON encode utf8, ascii7, utf16
 */

#if defined(__clang__)
  #define BW_NOINLINE __attribute__((noinline))
#elif defined(__GNUC__)
  #define BW_NOINLINE __attribute__((noinline))
#else
  #define BW_NOINLINE
#endif

namespace {
    struct BufferWriter {
        unsigned char* data;
        size_t size;
        size_t capacity;
    };

    BW_NOINLINE void actuallyGrow(BufferWriter* bw, size_t amount) {
        size_t newCapacity = bw->capacity;
        do {
            newCapacity *= 2;
        } while (newCapacity < bw->size + amount);
        
        unsigned char* newData = reinterpret_cast<unsigned char*>(realloc(bw->data, newCapacity));
        assert(newData);
        bw->data = newData;
        bw->capacity = newCapacity;
    }

    inline void grow(BufferWriter* bw, size_t amount) {
        if (bw->size + amount > bw->capacity) {
            actuallyGrow(bw, amount);
        }
    }
}

// assumes malloc will not fail
// TODO: replace assert with some other error mechanism

extern "C" BufferWriter* bw_new(size_t initialCapacity) {
    assert(initialCapacity >= 1);
    BufferWriter* bw = reinterpret_cast<BufferWriter*>(malloc(sizeof(BufferWriter)));
    assert(bw);
    bw->data = reinterpret_cast<unsigned char*>(malloc(initialCapacity));
    assert(bw->data);
    bw->size = 0;
    bw->capacity = initialCapacity;
    return bw;
}

extern "C" void bw_free(BufferWriter* bw) {
    free(bw->data);
    free(bw);
}

extern "C" size_t bw_get_size(BufferWriter* bw) {
    return bw->size;
}

extern "C" unsigned char* bw_trim_and_release_address(BufferWriter* bw) {
    unsigned char* data = bw->data;
    
    if (bw->size + TRIM_THRESHOLD < bw->capacity) {
        // try to shrink
        data = reinterpret_cast<unsigned char*>(realloc(data, bw->size));
        if (!data) {
            // no problem
            data = bw->data;
        }
    }

    bw->data = 0;
    return data;
}

extern "C" void bw_append_byte(BufferWriter* bw, unsigned char byte) {
    assert(bw->data);
    grow(bw, 1);
    bw->data[bw->size] = byte;
    bw->size += 1;
}

extern "C" void bw_append_char_utf8(BufferWriter* bw, uint32_t c) {
    assert(bw->data);
    grow(bw, 4);

    size_t size = bw->size;
    unsigned char* data = bw->data + size;

    if (c <= 0x7F) {
        data[0] = static_cast<unsigned char>(c);
        size += 1;
    } else if (c <= 0x7FF) {
        data[0] = static_cast<unsigned char>(0xC0 | (c >> 6));
        data[1] = static_cast<unsigned char>(0x80 | (c & 0x3F));
        size += 2;
    } else if (c <= 0xFFFF) {
        data[0] = static_cast<unsigned char>(0xE0 | (c >> 12));
        data[1] = static_cast<unsigned char>(0x80 | ((c >> 6) & 0x3F));
        data[2] = static_cast<unsigned char>(0x80 | (c & 0x3F));
        size += 3;
    } else if (c <= 0x1FFFFF) {
        data[0] = static_cast<unsigned char>(0xF0 | (c >> 18));
        data[1] = static_cast<unsigned char>(0x80 | ((c >> 12) & 0x3F));
        data[2] = static_cast<unsigned char>(0x80 | ((c >> 6) & 0x3F));
        data[3] = static_cast<unsigned char>(0x80 | (c & 0x3F));
        size += 4;
    } else {
        // Unicode characters out of this range are illegal...
        // should we assert?  or just ignore...
    }
    bw->size = size;
}

extern "C" void bw_append_bs(BufferWriter* bw, size_t size, const unsigned char* data) {
    assert(bw->data);
    grow(bw, size);

    memcpy(bw->data + bw->size, data, size);
    bw->size += size;
}

extern "C" void bw_append_bsz(BufferWriter* bw, const unsigned char* data) {
    bw_append_bs(bw, strlen(reinterpret_cast<const char*>(data)), data);
}

extern "C" void bw_append_byte7(BufferWriter* bw, unsigned char byte) {
    assert(bw->data);
    grow(bw, 1);
    bw->data[bw->size] = byte & 0x7F;
    bw->size += 1;
}

extern "C" void bw_append_bs7(BufferWriter* bw, size_t size, const unsigned char* data) {
    assert(bw->data);
    grow(bw, size);

    unsigned char* out = bw->data + bw->size;
    for (size_t i = 0; i < size; ++i) {
        out[i] = data[i] & 0x7F;
    }
    bw->size += size;
}

extern "C" void bw_append_bsz7(BufferWriter* bw, const unsigned char* data) {
    bw_append_bs7(bw, strlen(reinterpret_cast<const char*>(data)), data);
}

extern "C" void bw_append_json_escaped(BufferWriter* bw, size_t size, const unsigned char* data) {
    assert(bw->data);
    grow(bw, 2 + size * 2);

    unsigned char* dest = bw->data + bw->size;
    *dest++ = '\"';

    size_t i = 0;
    while (i < size) {
        switch (data[i]) {
            case '\n': *dest++ = '\\'; *dest++ = 'n';  break;
            case '\r': *dest++ = '\\'; *dest++ = 'r';  break;
            case '\t': *dest++ = '\\'; *dest++ = 't';  break;
            case '\\': *dest++ = '\\'; *dest++ = '\\'; break;
            case '\"': *dest++ = '\\'; *dest++ = '\"'; break;
            default:   *dest++ = data[i];
        }
        ++i;
    }

    *dest++ = '\"';
    bw->size = dest - bw->data;
}

unsigned bw_itoa(unsigned char* output_, signed int i) {
    unsigned char* output = output_;
    unsigned char buffer[32] = {0};
    unsigned char* p = buffer;

    // negation doesn't work if i == MIN_INT
    if (i < 0) {
        *output++ = '-';

        int io = i;
        i /= 10;
        *p++ = '0' + (i * 10 - io);
        if (i == 0) {
            *output++ = p[-1];
            return output - output_;
        } else {
            i = -i;
        }
    }

    do {
        *p++ = '0' + (i % 10);
        i /= 10;
    } while (i);

    while (p > buffer) {
        *output++ = *--p;
    }

    return output - output_;
}

extern "C" void bw_append_decimal_signed_int(BufferWriter* bw, signed int i) {
    assert(bw->data);
    grow(bw, 32); // enough

    unsigned used = bw_itoa(bw->data + bw->size, i);
    bw->size += used;
}

extern "C" void bw_append_decimal_double(BufferWriter* bw, double d) {
    assert(bw->data);
    grow(bw, 30); // ???

    size_t used = sprintf(reinterpret_cast<char*>(bw->data + bw->size), "%f", d);
    bw->size += used;
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
    unsigned char* utf8 = reinterpret_cast<unsigned char*>(malloc(size * 4));
    unsigned char* utf8end = utf8;

    bw_encode_utf8(&utf8end, words, 0, size);

    bw_append_json_escaped(bw, utf8end - utf8, utf8);

    free(utf8);
}
