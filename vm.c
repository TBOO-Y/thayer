#include <stdarg.h>
#include <stdio.h>

#include "common.h"
#include "compiler.h"
#include "object.h"
#include "memory.h"
#include "type_checker.h"
#include "vm.h"

#include <string.h>
#include <time.h>

#include "debug.h"

VM vm;

static Value clockNative(int argCount, Value* args) {
    return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
}

void resetStack() {
    vm.stack = GROW_ARRAY(Value, vm.stack, 0, 256); // Allocate memory for 256 Values initially to match init capacity
    vm.stackTop = vm.stack; // Set the count of the stack to 0
    vm.frameCount = 0;
}

static void runtimeError(const char* format, ...) {
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fputs("\n", stderr);

    for (int i = vm.frameCount - 1; i >= 0; i--) {
        CallFrame* frame = &vm.frames[i];
        ObjFunction* function = frame->function;
        size_t instruction = frame->ip - frame->function->chunk.code - 1;
        fprintf(stderr, "[line %d] in ", function->chunk.lines[instruction]);
        if (function->name == NULL) {
            fprintf(stderr, "script\n"); // This is kinda broken bc of my optimization, fix later
        } else {
            fprintf(stderr, "%s()\n", function->name->chars);
        }
    }

    resetStack();
}

static void defineNative(const char* name, NativeFn function, int arity) {
    push(OBJ_VAL(copyString(name, (int)strlen(name))));
    push(OBJ_VAL(newNative(function, arity)));
    tableSet(&vm.globals, AS_STRING(vm.stack[0]), vm.stack[1]);
    pop();
    pop();
}

void initVM() {
    vm.stackCapacity = 256; // Set stack capacity to 256 initially
    resetStack();
    vm.objects = NULL;
    initTable(&vm.globals);
    initTable(&vm.constGlobals);
    initTable(&vm.strings);

    defineNative("clock", clockNative, 0);
}

void freeVM() {
    freeTable(&vm.globals);
    freeTable(&vm.constGlobals);
    freeTable(&vm.strings);
    freeObjects();
}

void push(Value value) {
    if (vm.stackTop - vm.stack >= (long long)vm.stackCapacity) { // In reality, this code is reached only when ==
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

Value popN(uint8_t N) {
    vm.stackTop -= N;
    return *vm.stackTop;
}

Value peek(int distance) {
    return vm.stackTop[-1 - distance];
}

static bool call(ObjFunction* function, int argCount) {
    if (argCount != function->arity) {
        runtimeError("Expected %d arguments but got %d.", function->arity, argCount);
        return false;
    }

    if (vm.frameCount == FRAMES_MAX) {
        runtimeError("Stack overflow.");
        return false;
    }

    CallFrame* frame = &vm.frames[vm.frameCount++];
    frame->function = function;
    frame->ip = function->chunk.code;
    frame->slots = vm.stackTop - argCount - 1;
    return true;
}

static bool callValue(Value callee, int argCount) {
    if (IS_OBJ(callee)) {
        switch (OBJ_TYPE(callee)) {
            case OBJ_FUNCTION:
                return call(AS_FUNCTION(callee), argCount);
            case OBJ_NATIVE: {
                ObjNative* native = AS_NATIVE(callee);
                if (argCount != native->arity) {
                    runtimeError("Expected %d arguments but got %d.", native->arity, argCount);
                    return false;
                }
                NativeFn native_fn = native->function;
                Value result = native_fn(argCount, vm.stackTop - argCount);
                if (IS_ERR(result)) {
                    return false;
                }
                vm.stackTop -= argCount + 1;
                push(result);
                return true;
            }
            default:
                break; // Non-callable object type.
        }
    }
    runtimeError("Can only call functions and classes.");
    return false;
}

static bool isFalsey(Value value) {
    return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

static void concatenate() {
    ObjString* b = AS_STRING(pop());
    ObjString* a = AS_STRING(pop());
    int length = a->length + b->length;
    char* chars = ALLOCATE(char, length);
    memcpy(chars, a->chars, a->length);
    memcpy(chars + a->length, b->chars, b->length);
    chars[length] = '\0';

    ObjString* result = takeString(chars, length);
    push(OBJ_VAL(result));
}

static InterpretResult run() {
    CallFrame* frame = &vm.frames[vm.frameCount - 1];
#define READ_BYTE() (*frame->ip++)
#define READ_SHORT() \
    (frame->ip += 2, (uint16_t)((frame->ip[-2] << 8) | frame->ip[-1]))
#define READ_CONSTANT() (frame->function->chunk.constants.values[READ_BYTE()])
#define READ_STRING() AS_STRING(READ_CONSTANT())
#define BINARY_OP_EXECUTE(outputValueType, inputValueType, inputCType, op) \
    do { \
        if (!SAME_TYPE(peek(0), peek(1))) { \
            runtimeError("Operand types must be the same."); \
            return INTERPRET_RUNTIME_ERROR; \
        } \
        inputCType b = inputValueType(pop()); \
        inputCType a = inputValueType(pop()); \
        push(outputValueType(a op b)); \
    } while (false)
#define BINARY_OP(outputValueType, op) \
    do { \
        switch (peek(0).type) { \
            case VAL_CHAR:   BINARY_OP_EXECUTE(outputValueType, AS_CHAR, char, op); break; \
            case VAL_INT:    BINARY_OP_EXECUTE(outputValueType, AS_INT, int, op); break; \
            case VAL_NUMBER: BINARY_OP_EXECUTE(outputValueType, AS_NUMBER, double, op); break; \
            default: \
                runtimeError("Operands must be integers, doubles or chars."); \
                return INTERPRET_RUNTIME_ERROR; \
        } \
    } while (false)
#define BINARY_OP_ARITH(op) \
    do { \
        switch (peek(0).type) { \
            case VAL_CHAR:   BINARY_OP_EXECUTE(CHAR_VAL, AS_CHAR, char, op); break; \
            case VAL_INT:    BINARY_OP_EXECUTE(INT_VAL, AS_INT, int, op); break; \
            case VAL_NUMBER: BINARY_OP_EXECUTE(NUMBER_VAL, AS_NUMBER, double, op); break; \
            default: \
                runtimeError("Operands must be integers, doubles or chars."); \
                return INTERPRET_RUNTIME_ERROR; \
        } \
    } while (false)
#define BINARY_OP_BITWISE(op) \
    do { \
        if (peek(0).type == VAL_INT) { \
            BINARY_OP_EXECUTE(INT_VAL, AS_INT, int, op); \
        } else { \
            runtimeError("Operands must be integers."); \
            return INTERPRET_RUNTIME_ERROR; \
        } \
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
        disassembleInstruction(&frame->function->chunk, (int)(frame->ip - frame->function->chunk.code));
#endif

        uint8_t instruction;
        switch (instruction = READ_BYTE()) {
            case OP_CONSTANT: {
                Value constant = READ_CONSTANT();
                push(constant);
                break;
            }
            case OP_NIL:            push(NIL_VAL); break;
            case OP_TRUE:           push(BOOL_VAL(true)); break;
            case OP_FALSE:          push(BOOL_VAL(false)); break;
            case OP_POP:            pop(); break;
            case OP_POPN: {
                uint8_t N = READ_BYTE();
                popN(N);
                break;
            }
            case OP_GET_LOCAL: {
                uint8_t slot = READ_BYTE();
                push(frame->slots[slot]);
                break;
            }
            case OP_DEFINE_LOCAL: {
                TokenType type = READ_BYTE();
                if (!typeCheck(peek(0), type)) {
                    runtimeError("Tried to define local variable of type '%s' as value with type '%s'.",
                        typeToName(type), getValueTypeName(peek(0)));
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }
            case OP_SET_LOCAL: {
                uint8_t slot = READ_BYTE();
                if (!SAME_TYPE(peek(0), vm.stack[slot])
                    || IS_OBJ(peek(0)) && !SAME_OBJ_TYPE(peek(0), vm.stack[slot])) {
                    runtimeError("Tried to assign value of type '%s' to local variable of type '%s'.",
                        getValueTypeName(peek(0)), getValueTypeName(vm.stack[slot]));
                    return INTERPRET_RUNTIME_ERROR;
                }
                frame->slots[slot] = peek(0);
                break;
            }
            case OP_GET_GLOBAL: {
                ObjString* name = READ_STRING();
                Value value;
                if (!tableGet(&vm.globals, name, &value)) {
                    runtimeError("Undefined variable '%s'.", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
                push(value);
                break;
            }
            case OP_DEFINE_CONST_GLOBAL: {
                ObjString* name = READ_STRING();
                TokenType type = READ_BYTE();

                if (!typeCheck(peek(0), type)) {
                    const char* typeName = typeToName(type);
                    runtimeError("Variable '%s' does not match declared type '%s'.", name->chars, typeName);
                    return INTERPRET_RUNTIME_ERROR;
                }

                if (!tableSet(&vm.globals, name, peek(0))) {
                    runtimeError("Cannot re-define a constant global variable.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                tableSet(&vm.constGlobals, name, peek(0));
                pop();
                break;
            }
            case OP_DEFINE_GLOBAL: {
                ObjString* name = READ_STRING();
                TokenType type = READ_BYTE();
                Value value;

                if (!typeCheck(peek(0), type)) {
                    const char* typeName = typeToName(type);
                    runtimeError("Variable '%s' does not match declared type '%s'.", name->chars, typeName);
                    return INTERPRET_RUNTIME_ERROR;
                }

                if (tableGet(&vm.constGlobals, name, &value)) {
                    runtimeError("Cannot re-define a constant global variable.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                tableSet(&vm.globals, name, peek(0));
                pop();
                break;
            }
            case OP_SET_GLOBAL: { // Constant global operations all need to be optimized (divorce const globals table)
                ObjString* name = READ_STRING();
                Value value;
                if (tableGet(&vm.globals, name, &value) && !SAME_TYPE(peek(0), value)) {
                    runtimeError("Tried to assign value of type '%s' to variable '%s' of type '%s'.",
                        getValueTypeName(peek(0)), name->chars, getValueTypeName(value));
                    return INTERPRET_RUNTIME_ERROR;
                }

                if (tableSet(&vm.globals, name, peek(0))) {
                    tableDelete(&vm.globals, name);
                    runtimeError("Undefined variable '%s'.", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }

                if (tableGet(&vm.constGlobals, name, &value)) {
                    runtimeError("Cannot re-assign a constant global variable.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }
            case OP_TEST_EQUAL: {
                Value b = pop();
                Value a = peek(0);
                push(BOOL_VAL(valuesEqual(a, b)));
                break;
            }
            case OP_EQUAL: {
                Value b = pop();
                Value a = pop();
                push(BOOL_VAL(valuesEqual(a, b)));
                break;
            }
            case OP_NOT_EQUAL: {
                Value b = pop();
                Value a = pop();
                push(BOOL_VAL(!valuesEqual(a, b)));
                break;
            }
            case OP_GREATER:        BINARY_OP(BOOL_VAL, >); break;
            case OP_LESS:           BINARY_OP(BOOL_VAL, <); break;
            case OP_GREATER_EQUAL:  BINARY_OP(BOOL_VAL, >=); break;
            case OP_LESS_EQUAL:     BINARY_OP(BOOL_VAL, <=); break;
            case OP_ADD: {
                if (IS_STRING(peek(0)) && IS_STRING(peek(1))) {
                    concatenate();
                } else {
                    switch (peek(0).type) {
                        case VAL_CHAR:   BINARY_OP_EXECUTE(CHAR_VAL, AS_CHAR, char, +); break;
                        case VAL_INT:    BINARY_OP_EXECUTE(INT_VAL, AS_INT, int, +); break;
                        case VAL_NUMBER: BINARY_OP_EXECUTE(NUMBER_VAL, AS_NUMBER, double, +); break;
                        default:
                            runtimeError("Operands must be two numbers of the same type or two strings.");
                            return INTERPRET_RUNTIME_ERROR;
                    }
                }
                break;
            }
            case OP_SUBTRACT:       BINARY_OP_ARITH(-); break;
            case OP_MULTIPLY:       BINARY_OP_ARITH(*); break;
            case OP_DIVIDE:         BINARY_OP_ARITH(/); break;
            case OP_BITWISE_AND:    BINARY_OP_BITWISE(&); break;
            case OP_BITWISE_OR:     BINARY_OP_BITWISE(|); break;
            case OP_BITWISE_XOR:    BINARY_OP_BITWISE(^); break;
            case OP_BITWISE_NOT: {
                if (!IS_INT(peek(0))) {
                    runtimeError("Operand must be an integer.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                push(INT_VAL(~AS_INT(pop())));
            }
            case OP_NOT: {
                push(BOOL_VAL(isFalsey(pop())));
                break;
            }
            case OP_NEGATE: {
                if (!IS_NUMBER(peek(0))) {
                    runtimeError("Operand must be a number.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                push(NUMBER_VAL(-AS_NUMBER(pop())));
                break;
            }
            // case OP_NEGATE:   *(vm.stackTop - 1) = -(*(vm.stackTop - 1)); break;
            case OP_PRINT: {
                printValue(pop());
                printf("\n");
                break;
            }
            case OP_JUMP: {
                uint16_t offset = READ_SHORT();
                frame->ip += offset;
                break;
            }
            case OP_JUMP_IF_FALSE: {
                uint16_t offset = READ_SHORT();
                if (isFalsey(peek(0))) frame->ip += offset;
                break;
            }
            case OP_JUMP_IF_TRUE: {
                uint16_t offset = READ_SHORT();
                if (!isFalsey(peek(0))) frame->ip += offset;
                break;
            }
            case OP_LOOP: {
                uint16_t offset = READ_SHORT();
                frame->ip -= offset;
                break;
            }
            case OP_CALL: {
                int argCount = READ_BYTE();
                if (!callValue(peek(argCount), argCount)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                frame = &vm.frames[vm.frameCount - 1];
                break;
            }
            case OP_RETURN: {
                Value result = pop();
                vm.frameCount--;
                if (vm.frameCount == 0) {
                    pop();
                    return INTERPRET_OK;
                }

                vm.stackTop = frame->slots;
                push(result);
                frame = &vm.frames[vm.frameCount - 1];
                break;
            }
            case OP_TEST: {
                for (int i = 0; i< 257; i++) {
                    push(NUMBER_VAL(10));
                }
                break;
            }
        }
    }

#undef READ_BYTE
#undef READ_SHORT
#undef READ_CONSTANT
#undef READ_STRING
#undef BINARY_OP
}

InterpretResult interpret(const char* source) {
    ObjFunction* function = compile(source);
    if (function == NULL) return INTERPRET_COMPILE_ERROR;

    push(OBJ_VAL(function));
    call(function, 0);

    return run();
}