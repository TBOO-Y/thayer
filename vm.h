#ifndef thayer_vm_h
#define thayer_vm_h

#include "chunk.h"
#include "table.h"
#include "value.h"

typedef struct {
    Chunk* chunk;
    uint8_t* ip;
    Value* stack;
    Value* stackTop;
    int stackCapacity;
    Table globals;
    Table constGlobals;
    Table strings;
    Obj* objects;
} VM;

typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR
} InterpretResult;

extern VM vm;

void initVM();
void freeVM();
InterpretResult interpret(const char* source);
void push(Value value);
Value pop();

#endif
