#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

const size_t TRIM_THRESHOLD = 8192; // minimum number of bytes saved to trim

namespace {
    struct BufferWriter {
        unsigned char* data;
        size_t size;
        size_t capacity;
    };

    inline void grow(BufferWriter* bw, size_t amount) {
        if (bw->size + amount > bw->capacity) {
            size_t newCapacity = bw->capacity;
            do {
                newCapacity *= 2;
            } while (newCapacity < bw->size + amount);
            unsigned char* newData = reinterpret_cast<unsigned char*>(realloc(bw->data, newCapacity));
            assert(newData);
            bw->data = newData;
            bw->capacity = newCapacity;
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

extern "C" void bw_append_byte(BufferWriter* bw, unsigned char byte) {
    assert(bw->data);
    grow(bw, 1);
    bw->data[bw->size] = byte;
    bw->size += 1;
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

extern "C" void bw_append_json_escaped(BufferWriter* bw, size_t size, const unsigned char* data) {
    assert(bw->data);
    grow(bw, 2 + size * 2);

    unsigned char* dest = bw->data;
    *dest++ = '\"';

    size_t i = 0;
    while (i < size) {
        switch (data[i]) {
            case '\n': *dest++ = '\\'; *dest++ = 'r';  break;
            case '\r': *dest++ = '\\'; *dest++ = 'n';  break;
            case '\t': *dest++ = '\\'; *dest++ = 't';  break;
            case '\\': *dest++ = '\\'; *dest++ = '\\'; break;
            case '\"': *dest++ = '\\'; *dest++ = '\"'; break;
            default:   *dest++ = data[i];
        }
        ++i;
    }

    *dest++ = '\"';
    bw->size += dest - bw->data;
}
