#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>

namespace {
    struct BufferWriter {
        unsigned char* data;
        size_t size;
        size_t capacity;
    };
}

// assumes malloc cannot fail

extern "C" BufferWriter* bw_new(size_t initialCapacity) {
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
    if (bw->size >= bw->capacity) {
        size_t newCapacity = bw->capacity * 2;
        unsigned char* nd = reinterpret_cast<unsigned char*>(realloc(bw->data, newCapacity));
        assert(nd);
        bw->data = nd;
        bw->capacity = newCapacity;
    }

    bw->data[bw->size] = byte;
    bw->size += 1;
}

extern "C" void bw_append_bs(BufferWriter* bw, size_t size, unsigned char* data) {
    if (bw->size + size > bw->capacity) {
        size_t newCapacity = bw->capacity * 2;
        while (bw->size + size > newCapacity) {
            newCapacity *= 2;
        }
        unsigned char* nd = reinterpret_cast<unsigned char*>(realloc(bw->data, newCapacity));
        assert(nd);
        bw->data = nd;
        bw->capacity = newCapacity;
    }

    memcpy(bw->data + bw->size, data, size);
    bw->size += size;
}

extern "C" size_t bw_get_size(BufferWriter* bw) {
    return bw->size;
}

extern "C" unsigned char* bw_get_address(BufferWriter* bw) {
    return bw->data;
}

