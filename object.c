#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"
#include "vm.h"

#define ALLOCATE_OBJ(type, objectType) \
    (type*)allocateObject(sizeof(type), objectType)

static Obj* allocateObject(size_t size, ObjType type) {
    Obj* object = reallocate(NULL, 0, size);
    object->type = type;

    object->next = vm.objects;
    vm.objects = object;
    return object;
}

ObjFunction* newFunction() {
    ObjFunction* function = ALLOCATE_OBJ(ObjFunction, OBJ_FUNCTION);
    function->arity = 0;
    function->name = NULL;
    initChunk(&function->chunk);
    return function;
}

ObjNative* newNative(NativeFn function, int arity) {
    ObjNative* native = ALLOCATE_OBJ(ObjNative, OBJ_NATIVE);
    native->function = function;
    native->arity = arity;
    return native;
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

static inline uint32_t murmur_32_scramble(uint32_t k) {
    k *= 0xcc9e2d51;
    k = (k << 15) | (k >> 17);
    k *= 0x1b873593;
    return k;
}

uint32_t murmur3_32(const char* key, size_t len, uint32_t seed)
{
    uint32_t h = seed;
    uint32_t k;
    /* Read in groups of 4. */
    for (size_t i = len >> 2; i; i--) {
        // Here is a source of differing results across endiannesses.
        // A swap here has no effects on hash properties though.
        memcpy(&k, key, sizeof(uint32_t));
        key += sizeof(uint32_t);
        h ^= murmur_32_scramble(k);
        h = (h << 13) | (h >> 19);
        h = h * 5 + 0xe6546b64;
    }
    /* Read the rest. */
    k = 0;
    for (size_t i = len & 3; i; i--) {
        k <<= 8;
        k |= key[i - 1];
    }
    // A swap is *not* necessary here because the preceding loop already
    // places the low bytes in the low places according to whatever endianness
    // we use. Swaps only apply when the memory is copied in a chunk.
    h ^= murmur_32_scramble(k);
    /* Finalize. */
    h ^= len;
    h ^= h >> 16;
    h *= 0x85ebca6b;
    h ^= h >> 13;
    h *= 0xc2b2ae35;
    h ^= h >> 16;
    return h;
}

static uint32_t hashString(const char* key, int length) {
    if (sizeof(char) == sizeof(uint8_t)) {
        uint32_t seed = 0xbc9f1d34;
        return murmur3_32(key, length, seed);
    }

    uint32_t hash = 2166136261u;
    for (int i = 0; i< length; i++) {
        hash ^= (uint8_t)key[i];
        hash += 16777619;
    }
    return hash;
}

static uint32_t hashAddress(void* ptr) {
    uintptr_t number = (uintptr_t)ptr;
    uint32_t hash = 2166136261u;
    for (int i = 0; i < sizeof(number); i++) {
        uint8_t byte = (uint8_t)((number >> (i * 8)) & 0xFF);
        hash ^= byte;
        hash += 16777619;
    }
    return hash;
}

static uint32_t hashObject(Obj* obj) {
    switch (obj->type) {
        case OBJ_STRING:
            ObjString* str = (ObjString*)(obj);
            return hashString(str->chars, str->length);
        default:
            return hashAddress(&obj);
    }
}

static uint32_t hashValue(Value value) {
    switch (value.type) {
        case VAL_CHAR:   return (uint32_t)value.as.chara;
        case VAL_INT:    return (uint32_t)value.as.integer;
        case VAL_NUMBER: { // Verify this later, some sus stuff going on here
            double number = value.as.number;
            if (number == 0.0) return 0;
            // Re-interpret double bits as a long
            long bits = *(long*)(&number);
            // Mix bits using XOR
            return (uint32_t)(bits ^ (bits >> 32));
        }
        case VAL_OBJ:
            return hashObject(value.as.obj);
        default:
            return hashAddress(&value);
    }
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
    if (interned != NULL) return interned;

    char* heapChars = ALLOCATE(char, length + 1);
    memcpy(heapChars, chars, length);
    heapChars[length] = '\0';
    return allocateString(heapChars, length, hash);
}

static void printFunction(ObjFunction* function) {
    if (function->name == NULL) {
        printf("<script>");
        return;
    }
    printf("<fn %s>", function->name->chars);
}

void printObject(Value value) {
    switch (OBJ_TYPE(value)) {
        case OBJ_FUNCTION:
            printFunction(AS_FUNCTION(value));
        case OBJ_NATIVE:
            printf("<native fn>");
        break;
        case OBJ_STRING:
            printf("%s", AS_CSTRING(value));
            break;
    }
}
