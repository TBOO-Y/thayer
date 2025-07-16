#include <stdlib.h>
#include <stdio.h>

#include "chunk.h"
#include "memory.h"

void initChunk(Chunk* chunk) {
    chunk->count = 0;
    chunk->capacity = 0;
    chunk->code = NULL;
    chunk->lines = NULL;
    chunk->lineCompress = NULL;
    chunk->lineCount = 0;
    chunk->lineCapacity = 0;
    initValueArray(&chunk->constants);
}

void freeChunk(Chunk* chunk) {
    FREE_ARRAY(uint8_t, chunk->code, chunk->capacity);
    FREE_ARRAY(int, chunk->lines, chunk->lineCapacity);
    freeValueArray(&chunk->constants);
    initChunk(chunk);
}

void writeChunk(Chunk* chunk, uint8_t byte, int line) {
    if (chunk->lineCount == 0 || line != chunk->lines[chunk->lineCount - 1]) {
        if (chunk->lineCapacity < chunk->lineCount + 1) {
            int oldLineCapacity = chunk->lineCapacity;
            chunk->lineCapacity = GROW_CAPACITY(oldLineCapacity);
            chunk->lines = GROW_ARRAY(int, chunk->lines, oldLineCapacity, chunk->lineCapacity);
            chunk->lineCompress = GROW_ARRAY(int, chunk->lineCompress, oldLineCapacity, chunk->lineCapacity);
        }
        chunk->lines[chunk->lineCount] = line;
        chunk->lineCompress[chunk->lineCount] = 1;
        chunk->lineCount++;
    } else {
        chunk->lineCompress[chunk->lineCount - 1]++;
    }

    if (chunk->capacity < chunk->count + 1) { // If can't store the next thing, array doubling
        int oldCapacity = chunk->capacity;
        chunk->capacity = GROW_CAPACITY(oldCapacity);
        chunk->code = GROW_ARRAY(uint8_t, chunk->code, oldCapacity, chunk->capacity);
    }
    chunk->code[chunk->count] = byte;
    chunk->count++;
}

void writeConstant(Chunk* chunk, Value value, int line) {
    writeChunk(chunk, OP_CONSTANT_LONG, line);
    // Insert rest here.
}

int getLine(Chunk* chunk, int instruction) {
    int sum = 0;
    int count = 0;
    while (sum < instruction + 1 && count < chunk->lineCount) {
        sum += chunk->lineCompress[count];
        count++;
    }

    if (sum < instruction + 1) {
        return -1; // Error.
    }

    return chunk->lines[count - 1];
}

int addConstant(Chunk* chunk, Value value) {
    writeValueArray(&chunk->constants, value);
    return chunk->constants.count - 1;
}
