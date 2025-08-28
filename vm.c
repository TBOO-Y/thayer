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
    vm.openUpvalues = NULL;
}

static void runtimeError(const char* format, ...) {
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fputs("\n", stderr);

    for (int i = vm.frameCount - 1; i >= 0; i--) {
        CallFrame* frame = &vm.frames[i];
        ObjFunction* function = frame->closure->function;
        size_t instruction = frame->ip - frame->closure->function->chunk.code - 1;
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
    vm.bytesAllocated = 0;
    vm.nextGC = 1024 * 1024;

    vm.grayCount = 0;
    vm.grayCapacity = 0;
    vm.grayStack = NULL;

    initTable(&vm.globals);
    initTable(&vm.constGlobals);
    initTable(&vm.strings);

    vm.initString = NULL;
    vm.initString = copyString("init", 4);

    defineNative("clock", clockNative, 0);
}

void freeVM() {
    freeTable(&vm.globals);
    freeTable(&vm.constGlobals);
    freeTable(&vm.strings);
    vm.initString = NULL;
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

static bool call(ObjClosure* closure, int argCount) {
    if (argCount != closure->function->arity) {
        runtimeError("Expected %d arguments but got %d.", closure->function->arity, argCount);
        return false;
    }

    if (vm.frameCount == FRAMES_MAX) {
        runtimeError("Stack overflow.");
        return false;
    }

    CallFrame* frame = &vm.frames[vm.frameCount++];
    frame->closure = closure;
    frame->ip = closure->function->chunk.code;
    frame->slots = vm.stackTop - argCount - 1;
    return true;
}

static bool callValue(Value callee, int argCount) {
    if (IS_OBJ(callee)) {
        switch (OBJ_TYPE(callee)) {
            case OBJ_BOUND_METHOD: {
                ObjBoundMethod* bound = AS_BOUND_METHOD(callee);
                vm.stackTop[-argCount - 1] = bound->receiver;
                return call(bound->method, argCount);
            }
            case OBJ_CLASS: {
                ObjClass* klass = AS_CLASS(callee);
                vm.stackTop[-argCount - 1] = OBJ_VAL(newInstance(klass));
                Value initializer;
                if (tableGet(&klass->methods, vm.initString, &initializer)) {
                    return call(AS_CLOSURE(initializer), argCount);
                } else if (argCount != 0) {
                    runtimeError("Expected 0 arguments but got %d.", argCount);
                    return false;
                }
                return true;
            }
            case OBJ_CLOSURE:
                return call(AS_CLOSURE(callee), argCount);
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

static bool invokeFromClass(ObjClass* klass, ObjString* name, int argCount) {
    Value method;
    if (!tableGet(&klass->methods, name, &method)) {
        runtimeError("Undefined property '%s'.", name->chars);
        return false;
    }
    return call(AS_CLOSURE(method), argCount);
}

static bool invoke(ObjString* name, int argCount) {
    Value receiver = peek(argCount);

    if (!IS_INSTANCE(receiver)) {
        runtimeError("Only instances have methods.");
        return false;
    }

    ObjInstance* instance = AS_INSTANCE(receiver);

    Value value;
    if (tableGet(&instance->fields, name, &value)) {
        vm.stackTop[-argCount - 1] = value;
        return callValue(value, argCount);
    }

    return invokeFromClass(instance->klass, name, argCount);
}

static bool bindMethod(ObjClass* klass, ObjString* name) {
    Value method;
    if (!tableGet(&klass->methods, name, &method)) {
        runtimeError("Undefined property '%s'.", name->chars);
        return false;
    }

    ObjBoundMethod* bound = newBoundMethod(peek(0), AS_CLOSURE(method));

    pop();
    push(OBJ_VAL(bound));
    return true;
}

static ObjUpvalue* captureUpvalue(Value* local) {
    ObjUpvalue* prevUpvalue = NULL;
    ObjUpvalue* upvalue = vm.openUpvalues;
    while (upvalue != NULL && upvalue->location > local) {
        prevUpvalue = upvalue;
        upvalue = upvalue->next;
    }

    if (upvalue != NULL && upvalue ->location == local) {
        return upvalue;
    }

    ObjUpvalue* createdUpvalue = newUpvalue(local);
    createdUpvalue->next = upvalue;

    if (prevUpvalue == NULL) {
        vm.openUpvalues = createdUpvalue;
    } else {
        prevUpvalue->next = createdUpvalue;
    }

    return createdUpvalue;
}

static void closeUpvalues(Value* last) {
    while (vm.openUpvalues != NULL && vm.openUpvalues->location >= last) {
        ObjUpvalue* upvalue = vm.openUpvalues;
        upvalue->closed = *upvalue->location;
        upvalue->location = &upvalue->closed;
        vm.openUpvalues = upvalue->next;
    }
}

static void defineMethod(ObjString* name) {
    Value method = peek(0);
    ObjClass* klass = AS_CLASS(peek(1));
    tableSet(&klass->methods, name, method);
    pop();
}

static bool isFalsey(Value value) {
    return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

static void concatenate() {
    ObjString* b = AS_STRING(peek(0));
    ObjString* a = AS_STRING(peek(0));
    int length = a->length + b->length;
    char* chars = ALLOCATE(char, length);
    memcpy(chars, a->chars, a->length);
    memcpy(chars + a->length, b->chars, b->length);
    chars[length] = '\0';

    ObjString* result = takeString(chars, length);
    pop();
    pop();
    push(OBJ_VAL(result));
}

static InterpretResult run() {
    CallFrame* frame = &vm.frames[vm.frameCount - 1];
    register uint8_t* ip = frame->ip;
#define READ_BYTE() (*ip++)
#define READ_SHORT() \
    (ip += 2, (uint16_t)((ip[-2] << 8) | ip[-1]))
#define READ_CONSTANT() (frame->closure->function->chunk.constants.values[READ_BYTE()])
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
        disassembleInstruction(&frame->closure->function->chunk, (int)(ip - frame->closure->function->chunk.code));
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
                    if (!tableGet(&vm.constGlobals, name, &value)) {
                        runtimeError("Undefined variable '%s'.", name->chars);
                        return INTERPRET_RUNTIME_ERROR;
                    }
                }
                push(value);
                break;
            }
            case OP_DEFINE_CONST_GLOBAL: {
                ObjString* name = READ_STRING();
                TokenType type = READ_BYTE();
                Value value;

                if (!typeCheck(peek(0), type)) {
                    const char* typeName = typeToName(type);
                    runtimeError("Variable '%s' does not match declared type '%s'.", name->chars, typeName);
                    return INTERPRET_RUNTIME_ERROR;
                }

                if (tableGet(&vm.globals, name, &value)) {
                    runtimeError("Cannot re-define a global variable as constant.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                if (tableGet(&vm.constGlobals, name, &value)) {
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

                if (tableGet(&vm.globals, name, &value)) {
                    if (!SAME_TYPE(peek(0), value)) {
                        runtimeError("Tried to assign value of type '%s' to variable '%s' of type '%s'.",
                        getValueTypeName(peek(0)), name->chars, getValueTypeName(value));
                        return INTERPRET_RUNTIME_ERROR;
                    }
                    tableSet(&vm.globals, name, peek(0));
                } else {
                    if (tableGet(&vm.constGlobals, name, &value)) {
                        runtimeError("Tried to assign value to constant global variable '%s'.", name->chars);
                        return INTERPRET_RUNTIME_ERROR;
                    }
                    runtimeError("Undefined variable '%s'.", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }

                break;
            }
            case OP_GET_UPVALUE: {
                uint8_t slot = READ_BYTE();
                push(*frame->closure->upvalues[slot]->location);
                break;
            }
            case OP_SET_UPVALUE: {
                uint8_t slot = READ_BYTE();
                *frame->closure->upvalues[slot]->location = peek(0);
                break;
            }
            case OP_TEST_EQUAL: {
                Value b = pop();
                Value a = peek(0);
                push(BOOL_VAL(valuesEqual(a, b)));
                break;
            }
            case OP_GET_PROPERTY: {
                if (!IS_INSTANCE(peek(0))) {
                    runtimeError("Only instances have properties.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                ObjInstance* instance = AS_INSTANCE(peek(0));
                ObjString* name = READ_STRING();

                Value value;
                if (tableGet(&instance->fields, name, &value)) {
                    pop(); // Instance.
                    push(value);
                    break;
                }

                if (!bindMethod(instance->klass, name)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }
            case OP_SET_PROPERTY: {
                if (!IS_INSTANCE(peek(1))) {
                    runtimeError("Only instances have fields.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                ObjInstance* instance = AS_INSTANCE(peek(1));
                tableSet(&instance->fields, READ_STRING(), peek(0));
                Value value = pop();
                pop();
                push(value);
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
                ip += offset;
                break;
            }
            case OP_JUMP_IF_FALSE: {
                uint16_t offset = READ_SHORT();
                if (isFalsey(peek(0))) ip += offset;
                break;
            }
            case OP_JUMP_IF_TRUE: {
                uint16_t offset = READ_SHORT();
                if (!isFalsey(peek(0))) ip += offset;
                break;
            }
            case OP_LOOP: {
                uint16_t offset = READ_SHORT();
                ip -= offset;
                break;
            }
            case OP_CALL: {
                int argCount = READ_BYTE();
                if (!callValue(peek(argCount), argCount)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                frame->ip = ip;
                frame = &vm.frames[vm.frameCount - 1];
                ip = frame->ip;
                break;
            }
            case OP_INVOKE: {
                ObjString* method = READ_STRING();
                int argCount = READ_BYTE();
                if (!invoke(method, argCount)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                frame->ip = ip;
                frame = &vm.frames[vm.frameCount - 1];
                ip = frame->ip;
                break;
            }
            case OP_CLOSURE: {
                ObjFunction* function = AS_FUNCTION(READ_CONSTANT());
                ObjClosure* closure = newClosure(function);
                push(OBJ_VAL(closure));
                for (int i = 0; i < closure->upvalueCount; i++) {
                    uint8_t isLocal = READ_BYTE();
                    uint8_t index = READ_BYTE();
                    if (isLocal) {
                        closure->upvalues[i] = captureUpvalue(frame->slots + index);
                    } else {
                        closure->upvalues[i] = frame->closure->upvalues[index];
                    }
                }
                break;
            }
            case OP_CLOSE_UPVALUE: {
                closeUpvalues(vm.stackTop - 1);
                pop();
                break;
            }
            case OP_RETURN: {
                Value result = pop();
                closeUpvalues(frame->slots);
                vm.frameCount--;
                if (vm.frameCount == 0) {
                    pop();
                    return INTERPRET_OK;
                }

                vm.stackTop = frame->slots;
                push(result);
                frame = &vm.frames[vm.frameCount - 1];
                ip = frame->ip;
                break;
            }
            case OP_CLASS:
                push(OBJ_VAL(newClass(READ_STRING())));
                break;
            case OP_METHOD:
                defineMethod(READ_STRING());
                break;
            case OP_TEST: {
                for (int i = 0; i< 255; i++) {
                    push(NUMBER_VAL(16));
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
    ObjClosure* closure = newClosure(function);
    pop();
    push(OBJ_VAL(closure));
    call(closure, 0);

    return run();
}