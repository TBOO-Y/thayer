#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"
#include "vm.h"

/*
Copyright 2012-2024 JP Aumasson

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
documentation files (the "Software"), to deal in the Software without restriction, including without limitation the
rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit
persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the
Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

#define ALLOCATE_OBJ(type, objectType) \
    (type*)allocateObject(sizeof(type), objectType)

static Obj* allocateObject(size_t size, ObjType type) {
    Obj* object = reallocate(NULL, 0, size);
    object->type = type;

    object->next = vm.objects;
    vm.objects = object;
    return object;
}

static ObjString* allocateString(char* chars, int length, uint32_t hash) {
    ObjString* string = ALLOCATE_OBJ(ObjString, OBJ_STRING);
    string->length = length;
    string->chars = chars;
    string->hash = hash;
    string->freeable = true;
    tableSet(&vm.strings, string, NIL_VAL);
    return string;
}

// static inline uint32_t murmur_32_scramble(uint32_t k) {
//     k *= 0xcc9e2d51;
//     k = (k << 15) | (k >> 17);
//     k *= 0x1b873593;
//     return k;
// }
//
// uint32_t murmur3_32(const char* key, size_t len, uint32_t seed)
// {
//     uint32_t h = seed;
//     uint32_t k;
//     /* Read in groups of 4. */
//     for (size_t i = len >> 2; i; i--) {
//         // Here is a source of differing results across endiannesses.
//         // A swap here has no effects on hash properties though.
//         memcpy(&k, key, sizeof(uint32_t));
//         key += sizeof(uint32_t);
//         h ^= murmur_32_scramble(k);
//         h = (h << 13) | (h >> 19);
//         h = h * 5 + 0xe6546b64;
//     }
//     /* Read the rest. */
//     k = 0;
//     for (size_t i = len & 3; i; i--) {
//         k <<= 8;
//         k |= key[i - 1];
//     }
//     // A swap is *not* necessary here because the preceding loop already
//     // places the low bytes in the low places according to whatever endianness
//     // we use. Swaps only apply when the memory is copied in a chunk.
//     h ^= murmur_32_scramble(k);
//     /* Finalize. */
//     h ^= len;
//     h ^= h >> 16;
//     h *= 0x85ebca6b;
//     h ^= h >> 13;
//     h *= 0xc2b2ae35;
//     h ^= h >> 16;
//     return h;
// }
//
// static uint32_t hashString(const char* data, int length) {
//     uint32_t seed = 0xbc9f1d34;
//     return murmur3_32(data, length, seed);
// }

static uint32_t hashString(const char* key, int length) {
    uint32_t hash = 2166136261u;
    for (int i = 0; i< length; i++) {
        hash ^= (uint8_t)key[i];
        hash += 16777619;
    }
    return hash;
}

ObjString* allocateSourceString(const char* chars, int length) {
    uint32_t hash = hashString(chars, length);
    ObjString* string = ALLOCATE_OBJ(ObjString, OBJ_STRING);
    string->length = length;
    string->chars = chars;
    string->hash = hash;
    string->freeable = false;
    tableSet(&vm.strings, string, NIL_VAL);
    return string;
}

ObjString* takeString(char* chars, int length) {
    uint32_t hash = hashString(chars, length);
    ObjString* interned = tableFindString(&vm.strings, chars, length, hash);

    if (interned != NULL) {
        FREE_ARRAY(char, chars, length + 1);
        return interned;
    }

    return allocateString(chars, length, hash);
}

ObjString* copyString(const char* chars, int length) {
    uint32_t hash = hashString(chars, length);
    ObjString* interned = tableFindString(&vm.strings, chars, length, hash);
    if (interned == NULL) return interned;

    char* heapChars = ALLOCATE(char, length + 1);
    memcpy(heapChars, chars, length);
    heapChars[length] = '\0';
    return allocateString(heapChars, length, hash);
}

void printObject(Value value) {
    switch (OBJ_TYPE(value)) {
        case OBJ_STRING:
            printf("%s", AS_CSTRING(value));
            break;
    }
}
