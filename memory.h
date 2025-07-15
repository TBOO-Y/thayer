#ifndef thayer_memory_h
#define thayer_memory_h

#include "common.h"

// Must test minimum threshold of 8 further for GROW_CAPACITY
// For FREE_ARRAY, no need to cast since we're not using the return value

#define GROW_CAPACITY(capacity) \
    ((capacity < 8) ? 8 : (capacity) * 2)

#define GROW_ARRAY(type, pointer, oldCount, newCount) \
    (type*)reallocate(pointer, sizeof(type) * (oldCount), sizeof(type) * (newCount))

#define FREE_ARRAY(type, pointer, oldCount) \
    reallocate(pointer, sizeof(type) * (oldCount), 0)

void* reallocate(void* pointer, size_t oldSize, size_t newSize);

#endif