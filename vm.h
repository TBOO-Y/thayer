#ifndef thayer_vm_h
#define thayer_vm_h

#include "chunk.h"
#include "value.h"

typedef struct {
    Chunk* chunk;
    uint8_t* ip;
    Value* stack;
    Value* stackTop;
    int stackCapacity;
} VM;

typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
} InterpretResult;

void initVM();
void freeVM();
InterpretResult interpret(const char* source);
void push(Value value);
Value pop();

#endif
