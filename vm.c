#include <stdio.h>

#include "common.h"
#include "memory.h"
#include "vm.h"

#include "debug.h"

VM vm;

void resetStack() {
    vm.stack = GROW_ARRAY(Value, vm.stack, 0, 256); // Allocate memory for 256 Values initially to match init capacity
    vm.stackTop = vm.stack; // Set the count of the stack to 0
}

void initVM() {
    vm.stackCapacity = 256; // Set stack capacity to 256 initially
    resetStack();
}

void freeVM() {

}

void push(Value value) {
    if (vm.stackTop - vm.stack >= (long long)vm.stackCapacity) { // In reality, this code is reached only when they are ==
        int oldStackCapacity = vm.stackCapacity;
        vm.stackCapacity = GROW_CAPACITY(vm.stackCapacity);
        vm.stack = GROW_ARRAY(Value, vm.stack, oldStackCapacity, vm.stackCapacity);
        vm.stackTop = vm.stack + oldStackCapacity;
    }

    *vm.stackTop = value;
    vm.stackTop++;
}

Value pop() {
    vm.stackTop--;
    return *vm.stackTop;
}

static InterpretResult run() {
#define READ_BYTE() (*vm.ip++)
#define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()])
#define BINARY_OP(op) \
    do { \
        double b = pop(); \
        double a = pop(); \
        push(a op b); \
    } while (false)

    for (;;) {
#ifdef DEBUG_TRACE_EXECUTION
        printf("       ");
        for (Value* slot = vm.stack; slot < vm.stackTop; slot++) {
            printf("[ ");
            printValue(*slot);
            printf(" ]");
        }
        printf("\n");
        disassembleInstruction(vm.chunk, (int)(vm.ip - vm.chunk->code));
#endif

        uint8_t instruction;
        switch (instruction = READ_BYTE()) {
            case OP_CONSTANT: {
                Value constant = READ_CONSTANT();
                push(constant);
                break;
            }
            case OP_ADD:      BINARY_OP(+); break;
            case OP_SUBTRACT: BINARY_OP(-); break;
            case OP_MULTIPLY: BINARY_OP(*); break;
            case OP_DIVIDE:   BINARY_OP(/); break;
            case OP_NEGATE:   push(-pop()); break;
            // case OP_NEGATE:   *(vm.stackTop - 1) = -(*(vm.stackTop - 1)); break;
            case OP_RETURN: {
                printValue(pop());
                printf("\n");
                return INTERPRET_OK;
            }
            case OP_TEST: {
                for (int i = 0; i< 257; i++) {
                    push((Value)10);
                }
                break;
            }
        }
    }

#undef READ_BYTE
#undef READ_CONSTANT
#undef BINARY_OP
}

InterpretResult interpret(Chunk* chunk) {
    vm.chunk = chunk;
    vm.ip = chunk->code;
    return run();
}