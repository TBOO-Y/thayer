#include <stdio.h>
#include "common.h"
#include "chunk.h"
#include "debug.h"

int main(int argc, const char* argv[]) { // This is a test.
    Chunk chunk;
    initChunk(&chunk);

    int constant = addConstant(&chunk, 1.2);
    writeChunk(&chunk, OP_CONSTANT, 123);
    writeChunk(&chunk, constant, 123);
    int newConstant = addConstant(&chunk, 3.4);
    writeChunk(&chunk, OP_CONSTANT, 123);
    writeChunk(&chunk, newConstant, 123);
    int yetAnotherConstant = addConstant(&chunk, 5.6);
    writeChunk(&chunk, OP_CONSTANT, 124);
    writeChunk(&chunk, yetAnotherConstant, 124);

    writeChunk(&chunk, OP_RETURN, 124);

    disassembleChunk(&chunk, "test chunk");
    // printf("I am here");
    freeChunk(&chunk);
    return 0;
}
