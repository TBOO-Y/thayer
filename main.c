#include <stdio.h>
#include <time.h> // Only for testing purposes

#include "common.h"
#include "chunk.h"
#include "debug.h"
#include "vm.h"

int main(int argc, const char* argv[]) { // This is a test.
    initVM();

    Chunk chunk;
    initChunk(&chunk);

    int constant = addConstant(&chunk, 1);
    writeChunk(&chunk, OP_CONSTANT, 123);
    writeChunk(&chunk, constant, 123);

    // writeChunk(&chunk, OP_TEST, 123);

    writeChunk(&chunk, OP_RETURN, 124);

    // disassembleChunk(&chunk, "test chunk");
    // clock_t startTime = clock();
    interpret(&chunk);
    // clock_t timeElapsed = clock() - startTime;
    // printf("%f", (float)timeElapsed / CLOCKS_PER_SEC);
    // printf("I am here");
    freeVM();
    freeChunk(&chunk);
    return 0;
}
