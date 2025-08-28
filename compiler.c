#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "object.h"
#include "vm.h"
#include "common.h"
#include "compiler.h"

#include <math.h>

#include "memory.h"
#include "scanner.h"
#include "type_checker.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

typedef struct {
    Token current;
    Token previous;
    bool hadError;
    bool panicMode;
} Parser;

typedef enum {
    PREC_NONE,
    PREC_ASSIGNMENT,  // =
    PREC_OR,          // or
    PREC_AND,         // and
    PREC_BITWISE_OR,  // |
    PREC_BITWISE_XOR, // ^
    PREC_BITWISE_AND, // &
    PREC_EQUALITY,    // == !=
    PREC_COMPARISON,  // < > <= >=
    PREC_TERM,        // + -
    PREC_FACTOR,      // * /
    PREC_POWER,       // **
    PREC_UNARY,       // ! - ~
    PREC_CALL,        // . ()
    PREC_PRIMARY
} Precedence;

typedef void (*ParseFn)(bool canAssign);

typedef struct {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

typedef struct {
    Token name;
    int depth;
    bool isConst;
    bool isCaptured;
} Local;

typedef struct {
    uint8_t index;
    bool isLocal;
} Upvalue;

typedef enum {
    TYPE_FUNCTION,
    TYPE_INITIALIZER,
    TYPE_METHOD,
    TYPE_SCRIPT
} FunctionType;

typedef struct {
    int* array;
    int capacity;
    int count;
} IntArray;

typedef struct Compiler {
    struct Compiler* enclosing; // Implement a limit on how many enclosing compilers possible
    ObjFunction* function;
    FunctionType type;

    Local locals[UINT8_COUNT];
    int localCount;
    Upvalue upvalues[UINT8_COUNT];
    int scopeDepth;
    IntArray loopBreaks;
    IntArray loopContinues;
    IntArray loops;
} Compiler;

typedef struct ClassCompiler {
    struct ClassCompiler* enclosing;
} ClassCompiler;

Parser parser;
Compiler* current = NULL; // Change later for adaptability to multi-threaded applications, see Chapter 22
ClassCompiler* currentClass = NULL;
Chunk* compilingChunk;

static void initIntArray(IntArray* intArray) {
    intArray->array = NULL;
    intArray->capacity = 0;
    intArray->count = 0;
}

static void writeIntArray(IntArray* intArray, int value) {
    if (intArray->capacity < intArray->count + 1) {
        int oldCapacity = intArray->capacity;
        intArray->capacity = GROW_CAPACITY(oldCapacity);
        intArray->array = GROW_ARRAY(int, intArray->array, oldCapacity, intArray->capacity);
    }

    intArray->array[intArray->count] = value;
    intArray->count++;
}

static void deleteIntArray(IntArray* intArray) {
    intArray->count--;
}

static void freeIntArray(IntArray* intArray) {
    FREE_ARRAY(int, intArray->array, intArray->capacity);
    initIntArray(intArray);
}

static Chunk* currentChunk() {
    return &current->function->chunk;
}

static void errorAt(Token* token, const char* message) {
    if (parser.panicMode) return;
    parser.panicMode = true;
    fprintf(stderr, "[line %d] Error", token->line);

    if (token->type == TOKEN_EOF) {
        fprintf(stderr, " at end");
    } else if (token->type == TOKEN_ERROR) {
        // Nothing.
    } else {
        fprintf(stderr, " at '%.*s'", token->length, token->start);
    }

    fprintf(stderr, ": %s\n", message);
    parser.hadError = true;
}

static void error(const char* message) {
    errorAt(&parser.previous, message);
}

static void errorAtCurrent(const char* message) {
    errorAt(&parser.current, message);
}

static void advance() {
    parser.previous = parser.current;

    for (;;) {
        parser.current = scanToken();
        if (parser.current.type != TOKEN_ERROR) break;

        errorAtCurrent(parser.current.start);
    }
}

static void consume(TokenType type, const char* message) {
    if (parser.current.type == type) {
        advance();
        return;
    }

    errorAtCurrent(message);
}

static TokenType typeConsume(const char* message) {
    if (parser.current.type == TOKEN_INT || parser.current.type == TOKEN_STR ||
        parser.current.type == TOKEN_DOUBLE || parser.current.type == TOKEN_BOOL ||
        parser.current.type == TOKEN_VAR || parser.current.type == TOKEN_CHAR ||
        parser.current.type == TOKEN_FUN) {
        advance();
        return parser.previous.type;
    }

    errorAtCurrent(message);
    return TOKEN_ERROR;
}

static bool check(TokenType type) {
    return parser.current.type == type;
}

static bool match(TokenType type) {
    if (!check(type)) return false;
    advance();
    return true;
}

static void emitByte(uint8_t byte) {
    writeChunk(currentChunk(), byte, parser.previous.line);
}


static void emitBytes(uint8_t byte1, uint8_t byte2) {
    emitByte(byte1);
    emitByte(byte2);
}

static void emitLoop(int loopStart) {
    emitByte(OP_LOOP);

    int offset = currentChunk()->count - loopStart + 2;
    if (offset > UINT16_MAX) error("Loop body too large.");

    emitByte((offset >> 8 & 0xff));
    emitByte(offset & 0xff);
}

static int emitJump(uint8_t instruction) {
    emitByte(instruction);
    emitByte(0xff);
    emitByte(0xff);
    return currentChunk()->count - 2;
}

static void emitReturn() {
    if (current->type == TYPE_INITIALIZER) {
        emitBytes(OP_GET_LOCAL, 0);
    } else {
        emitByte(OP_NIL);
    }

    emitByte(OP_RETURN);
}

static uint8_t makeConstant(Value value) { // Again, revisit this for OP_CONSTANT_LONG
    int constant = addConstant(currentChunk(), value);
    if (constant > UINT8_MAX) {
        error("Too many constants in one chunk.");
        return 0;
    }

    return (uint8_t)constant;
}

static void emitConstant(Value value) {
    emitBytes(OP_CONSTANT, makeConstant(value));
}

static void patchJump(int offset) {
    // -2 to adjust for the bytecode for the jump offset itself.
    int jump = currentChunk()->count - offset - 2;

    if (jump > UINT16_MAX) {
        error("Too much code to jump over.");
    }

    currentChunk()->code[offset] = (jump >> 8) & 0xff;
    currentChunk()->code[offset + 1] = jump & 0xff;
}

static void initCompiler(Compiler* compiler, FunctionType type) {
    compiler->enclosing = current;
    compiler->function = NULL;
    compiler->type = type;
    compiler->localCount = 0;
    compiler->scopeDepth = 0;
    compiler->function = newFunction();
    initIntArray(&compiler->loopBreaks);
    initIntArray(&compiler->loopContinues);
    initIntArray(&compiler->loops);
    current = compiler;
    if (type != TYPE_SCRIPT) {
        current->function->name = copyString(parser.previous.start, parser.previous.length);
    }

    Local* local = &current->locals[current->localCount++];
    local->depth = 0;
    local->isCaptured = false;
    local->isConst = false;
    if (type != TYPE_FUNCTION) {
        local->name.start = "this";
        local->name.length = 4;
    } else {
        local->name.start = "";
        local->name.length = 0;
    }
}

static ObjFunction* endCompiler() {
    freeIntArray(&current->loopBreaks);
    freeIntArray(&current->loopContinues);
    freeIntArray(&current->loops);
    emitReturn();
    ObjFunction* function = current->function;
#ifdef DEBUG_PRINT_CODE
    if (!parser.hadError) {
        disassembleChunk(currentChunk(), function->name != NULL
            ? function->name->chars : "<script>");
    }
#endif

    current = current->enclosing;
    return function;
}

static void beginScope() {
    current->scopeDepth++;
}

static void endScope() {
    current->scopeDepth--;
    // uint8_t numberPopped = 0;
    while (current->localCount > 0 &&
           current->locals[current->localCount - 1].depth > current->scopeDepth) {
        if (current->locals[current->localCount - 1].isCaptured) {
            emitByte(OP_CLOSE_UPVALUE);
        } else {
            emitByte(OP_POP);
        }
        // numberPopped++;
        current->localCount--;
    }
    // emitBytes(OP_POPN, numberPopped);
}

static void expression();
static void statement();
static void declaration();
static ParseRule* getRule(TokenType type);
static void parsePrecedence(Precedence precedence);
static void namedVariable(Token name, bool canAssign);

static uint8_t identifierConstant(Token* name) {
    return makeConstant(OBJ_VAL(copyString(name->start, name->length))); // Turn to allocate source string later
}

static bool identifiersEqual(Token* a, Token* b) {
    if (a->length != b->length) return false;
    return memcmp(a->start, b->start, a->length) == 0;
}

static int resolveLocal(Compiler* compiler, Token* name) {
    for (int i = compiler->localCount - 1; i >= 0; i--) {
        Local* local = &compiler->locals[i];
        if (identifiersEqual(name, &local->name)) {
            if (local->depth == -1) {
                error("Can't read local variable in its own initializer.");
            }
            return i;
        }
    }

    return -1;
}

static int addUpvalue(Compiler* compiler, uint8_t index, bool isLocal) {
    int upvalueCount = compiler->function->upvalueCount;

    for (int i = 0; i < upvalueCount; i++) {
        Upvalue* upvalue = &compiler->upvalues[i];
        if (upvalue->index == index && upvalue->isLocal == isLocal) {
            return i;
        }
    }

    if (upvalueCount == UINT8_COUNT) {
        error("Too many closure variables in function.");
        return 0;
    }

    compiler->upvalues[upvalueCount].isLocal = isLocal;
    compiler->upvalues[upvalueCount].index = index;
    return compiler->function->upvalueCount++;
}

static int resolveUpvalue(Compiler* compiler, Token* name) {
    if (compiler->enclosing == NULL) return -1;

    int local = resolveLocal(compiler->enclosing, name);
    if (local != -1) {
        compiler->enclosing->locals[local].isCaptured = true;
        return addUpvalue(compiler, (uint8_t)local, true);
    }

    int upvalue = resolveUpvalue(compiler->enclosing, name);
    if (upvalue != -1) {
        return addUpvalue(compiler, (uint8_t)upvalue, false);
    }

    return -1;
}

static void addLocal(Token name, bool isConst) {
    if (current->localCount == UINT8_COUNT) {
        error("Too many local variables in function.");
        return;
    }

    Local* local = &current->locals[current->localCount++];
    local->name = name;
    local->depth = -1;
    local->isConst = isConst;
    local->isCaptured = false;
}

static void declareVariable(bool isConst) {
    if (current->scopeDepth == 0) return;

    Token* name = &parser.previous;
    for (int i = current->localCount - 1; i >= 0; i--) {
        Local* local = &current->locals[i];
        if (local->depth != -1 && local->depth < current->scopeDepth) {
            break;
        }

        if (identifiersEqual(name, &local->name)) {
            error("Already a variable with this name in the scope.");
        }
    }

    addLocal(*name, isConst);
}

static uint8_t parseVariable(const char* errorMessage, bool isConst) {
    consume(TOKEN_IDENTIFIER, errorMessage);

    declareVariable(isConst);
    if (current->scopeDepth > 0) return 0;

    return identifierConstant(&parser.previous);
}

static void markInitialized() {
    if (current->scopeDepth == 0) return;
    current->locals[current->localCount - 1].depth = current->scopeDepth;
}

static void defineVariable(uint8_t global, TokenType type, bool isConst) {
    if (current->scopeDepth > 0) {
        markInitialized();
        emitBytes(OP_DEFINE_LOCAL, type);
        return;
    }

    if (isConst) {
        emitBytes(OP_DEFINE_CONST_GLOBAL, global);
        emitByte(type);
        return;
    }
    emitBytes(OP_DEFINE_GLOBAL, global);
    emitByte(type);
}

static uint8_t argumentList() {
    uint8_t argCount = 0;
    if (!check(TOKEN_RIGHT_PAREN)) {
        do {
            expression();
            if (argCount == 255) {
                error("Can't have more than 255 arguments.");
            }
            argCount++;
        } while (match(TOKEN_COMMA));
    }
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
    return argCount;
}

static void _and(bool canAssign) {
    int endJump = emitJump(OP_JUMP_IF_FALSE);

    emitByte(OP_POP);
    parsePrecedence(PREC_AND);

    patchJump(endJump);
}

static void binary(bool canAssign) {
    TokenType operatorType = parser.previous.type;
    ParseRule* rule = getRule(operatorType);
    parsePrecedence((Precedence)(rule->precedence + 1));

    switch (operatorType) { // Not sure whether to make += expand in compile time or not yet
        case TOKEN_BANG_EQUAL:    emitByte(OP_NOT_EQUAL); break;
        case TOKEN_EQUAL_EQUAL:   emitByte(OP_EQUAL); break;
        case TOKEN_GREATER:       emitByte(OP_GREATER); break;
        case TOKEN_GREATER_EQUAL: emitByte(OP_GREATER_EQUAL); break;
        case TOKEN_LESS:          emitByte(OP_LESS); break;
        case TOKEN_LESS_EQUAL:    emitByte(OP_LESS_EQUAL); break;
        case TOKEN_PLUS:          emitByte(OP_ADD); break;
        case TOKEN_MINUS:         emitByte(OP_SUBTRACT); break;
        case TOKEN_STAR:          emitByte(OP_MULTIPLY); break;
        case TOKEN_SLASH:         emitByte(OP_DIVIDE); break;
        case TOKEN_AMPERSAND:     emitByte(OP_BITWISE_AND); break;
        case TOKEN_VERTICAL_LINE: emitByte(OP_BITWISE_OR); break;
        case TOKEN_CARET:         emitByte(OP_BITWISE_XOR); break;
        case TOKEN_STAR_STAR:     emitByte(OP_POWER); break;
        default:
            return; // Unreachable.
    }
}

static void call(bool canAssign) {
    uint8_t argCount = argumentList();
    emitBytes(OP_CALL, argCount);
}

static void dot(bool canAssign) {
    consume(TOKEN_IDENTIFIER, "Expect property name after '.'.");
    uint8_t name = identifierConstant(&parser.previous);

    if (canAssign && match(TOKEN_EQUAL)) {
        expression();
        emitBytes(OP_SET_PROPERTY, name);
    } else if (match(TOKEN_LEFT_PAREN)) {
        uint8_t argCount = argumentList();
        emitBytes(OP_INVOKE, name);
        emitByte(argCount);
    } else {
        emitBytes(OP_GET_PROPERTY, name);
    }
}

static void literal(bool canAssign) {
    switch (parser.previous.type) {
        case TOKEN_FALSE: emitByte(OP_FALSE); break;
        case TOKEN_NIL: emitByte(OP_NIL); break;
        case TOKEN_TRUE: emitByte(OP_TRUE); break;
        default: return; // Unreachable.
    }
}

static void grouping(bool canAssign) {
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void expression() {
    parsePrecedence(PREC_ASSIGNMENT);
}

static void block() {
    while (!check(TOKEN_END_BLOCK) && !check(TOKEN_EOF)) {
        declaration();
    }

    consume(TOKEN_END_BLOCK, "Expect 'eblock' after block.");
}

static void ifControlBlock() {
    while (!check(TOKEN_END) && !check(TOKEN_ELSE) && !check(TOKEN_EOF)) {
        declaration();
    }

    if (check(TOKEN_ELSE)) return;
    consume(TOKEN_END, "Expect 'end' after conditional or loop.");
}

static void controlBlock() {
    while (!check(TOKEN_END) && !check(TOKEN_EOF)) {
        declaration();
    }

    consume(TOKEN_END, "Expect 'end' after function, conditional, or loop.");
}

static void function(FunctionType type) {
    Compiler compiler;
    initCompiler(&compiler, type);
    beginScope();

    consume(TOKEN_LEFT_PAREN, "Expect '(' after function name.");
    if (!check(TOKEN_RIGHT_PAREN)) {
        do {
            current->function->arity++;
            if (current->function->arity > 255) {
                errorAtCurrent("Can't have more than 255 parameters.");
            }
            TokenType paramType = typeConsume("Expect parameter type."); // Add support for constant param later
            uint8_t constant = parseVariable("Expect parameter name.", false);
            defineVariable(constant, paramType, false);
        } while (match(TOKEN_COMMA));
    }
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
    consume(TOKEN_AS, "Expect 'as' before function body.");
    controlBlock();

    ObjFunction* function = endCompiler();
    emitBytes(OP_CLOSURE, makeConstant(OBJ_VAL(function)));

    for (int i = 0; i < function->upvalueCount; i++) {
        emitByte(compiler.upvalues[i].isLocal ? 1 : 0);
        emitByte(compiler.upvalues[i].index);
    }
}

static void method() {
    consume(TOKEN_IDENTIFIER, "Expect method name.");
    uint8_t constant = identifierConstant(&parser.previous);

    FunctionType type = TYPE_METHOD;
    if (parser.previous.length == 4 && memcmp(parser.previous.start, "init", 4) == 0) {
        type = TYPE_INITIALIZER;
    }

    function(type);
    emitBytes(OP_METHOD, constant);
}

static void classDeclaration() {
    consume(TOKEN_IDENTIFIER, "Expect class name.");
    Token className = parser.previous;
    uint8_t nameConstant = identifierConstant(&parser.previous);
    declareVariable(false);

    emitBytes(OP_CLASS, nameConstant);
    defineVariable(nameConstant, TOKEN_CLASS, false);

    ClassCompiler classCompiler;
    classCompiler.enclosing = currentClass;
    currentClass = &classCompiler;

    namedVariable(className, false);
    consume(TOKEN_AS, "Expect 'as' before class body."); // Add field declarations later
    while (!check(TOKEN_END) && !check(TOKEN_EOF)) {
        method();
    }
    consume(TOKEN_END, "Expect 'end' after class body.");
    emitByte(OP_POP);

    currentClass = currentClass->enclosing;
}

static void funDeclaration() {
    uint8_t global = parseVariable("Expect function name.", false);
    markInitialized();
    function(TYPE_FUNCTION);
    defineVariable(global, TOKEN_FUN, false);
}

static void varDeclaration(bool isConst, bool loop) {
    TokenType type = parser.previous.type;

    uint8_t global = parseVariable("Expect variable name.", isConst);

    if (match(TOKEN_EQUAL)) {
        expression();
    } else {
        emitByte(OP_NIL);
    }
    if (loop) {
        consume(TOKEN_COLON, "Expect : after variable declaration.");
    } else {
        consume(TOKEN_SEMICOLON, "Expect ; after variable declaration.");
    }

    defineVariable(global, type, isConst);
}

static void constVarDeclaration() {
    if (match(TOKEN_VAR) || match(TOKEN_INT) // Seems slow, should find a better way later
        || match(TOKEN_STR) || match(TOKEN_DOUBLE)
        || match(TOKEN_BOOL) || match(TOKEN_CHAR)
        || match(TOKEN_FUN)) {
        varDeclaration(true, false);
    } else {
        errorAtCurrent("Type annotation must follow 'const' keyword.");
    }
}

static void breakStatement() {
    if (current->loops.count == 0) {
        errorAtCurrent("Break statements must be in loops.");
    }

    int breakJump = emitJump(OP_JUMP);
    writeIntArray(&current->loopBreaks, breakJump);

    consume(TOKEN_SEMICOLON, "Expect ; after break statement.");
}

static void continueStatement() {
    if (current->loops.count == 0) {
        errorAtCurrent("Continue statements must be in loops.");
    }

    int continueJump = emitJump(OP_JUMP);
    writeIntArray(&current->loopContinues, continueJump);

    consume(TOKEN_SEMICOLON, "Expect ; after continue statement.");
}

static void expressionStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ; after expression.");
    emitByte(OP_POP);
}

static void forStatement() {
    beginScope();
    if (match(TOKEN_COLON)) {
        // No initializer.
    } else if (match(TOKEN_VAR) || match(TOKEN_INT) // Seems slow, should find a better way later
        || match(TOKEN_STR) || match(TOKEN_DOUBLE)
        || match(TOKEN_BOOL) || match(TOKEN_CHAR)
        || match(TOKEN_FUN)) {
        varDeclaration(false, true);
    } else {
        expressionStatement();
    }

    int loopStart = currentChunk()->count;
    int exitJump = -1;
    if (!match(TOKEN_COLON)) {
        expression();
        consume(TOKEN_COLON, "Expect ':' after loop condition.");

        // Jump out of the loop if the condition is false.
        exitJump = emitJump(OP_JUMP_IF_FALSE);
        emitByte(OP_POP); // Condition.
    }
    // consume(TOKEN_COLON, "Expect ':'.");
    if (!match(TOKEN_DO)) {
        int bodyJump = emitJump(OP_JUMP);
        int incrementStart = currentChunk()->count;
        expression();
        emitByte(OP_POP);
        consume(TOKEN_DO, "Expect do after for clauses.");

        emitLoop(loopStart);
        loopStart = incrementStart;
        patchJump(bodyJump);
    }

    writeIntArray(&current->loops, currentChunk()->count);
    controlBlock();
    emitLoop(loopStart);

    while (current->loopBreaks.count > 0 && current->loopBreaks.array[current->loopBreaks.count - 1] >
           current->loops.array[current->loops.count - 1]) {
        patchJump(current->loopBreaks.array[current->loopBreaks.count - 1]);
        current->loopBreaks.count--;
    }
    current->loops.count--;

    if (exitJump != -1) {
        patchJump(exitJump);
        emitByte(OP_POP); // Condition.
    }

    endScope();
}

static void ifStatement() {
    expression();
    consume(TOKEN_THEN, "Expect 'then' after condition.");

    int thenJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP);
    ifControlBlock();

    int elseJump = emitJump(OP_JUMP);
    patchJump(thenJump);
    emitByte(OP_POP);

    IntArray toElseJumps;
    initIntArray(&toElseJumps);

    while (match(TOKEN_ELSE) && check(TOKEN_IF)) {
        advance();
        expression();
        consume(TOKEN_THEN, "Expect 'then' after condition.");

        int elseIfJump = emitJump(OP_JUMP_IF_FALSE);
        emitByte(OP_POP);
        ifControlBlock();

        int toElseJump = emitJump(OP_JUMP);
        patchJump(elseIfJump);
        emitByte(OP_POP);

        writeIntArray(&toElseJumps, toElseJump);
    }

    if (parser.previous.type == TOKEN_ELSE) {
        controlBlock();
    }
    patchJump(elseJump);

    for (int i = 0; i < toElseJumps.count; i++) {
        patchJump(toElseJumps.array[i]);
    }

    freeIntArray(&toElseJumps);
}

static void printStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ; after value.");
    emitByte(OP_PRINT);
}

static void returnStatement() {
    if (current->type == TYPE_SCRIPT) {
        error("Can't return from top-level code.");
    }

    if (match(TOKEN_SEMICOLON)) {
        emitReturn();
    } else {
        if (current->type == TYPE_INITIALIZER) {
            error("Can't return a value from an initializer.");
        }

        expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after return value.");
        emitByte(OP_RETURN);
    }
}

static void switchStatement() { // Maybe a subtle bug: I pop after the entire logic is done, not right after the switch.
    expression();
    consume(TOKEN_THEN, "Expect 'then' after switch expression.");

    IntArray toAfterJumps;
    initIntArray(&toAfterJumps);

    while (match(TOKEN_CASE)) {
        expression();
        consume(TOKEN_COLON, "Expect ':' after case.");

        emitByte(OP_TEST_EQUAL);
        int caseJump = emitJump(OP_JUMP_IF_FALSE);
        emitByte(OP_POP);
        controlBlock();

        int toAfterJump = emitJump(OP_JUMP);
        patchJump(caseJump);
        emitByte(OP_POP);

        writeIntArray(&toAfterJumps, toAfterJump);
    }

    if (match(TOKEN_DEFAULT)) {
        consume(TOKEN_COLON, "Expect ':' after default case.");
        controlBlock();
    }

    for (int i = 0; i < toAfterJumps.count; i++) {
        patchJump(toAfterJumps.array[i]);
    }

    freeIntArray(&toAfterJumps);
    emitByte(OP_POP); // To pop the initial expression.
}

static void whileStatement() {
    int loopStart = currentChunk()->count;
    expression();
    consume(TOKEN_DO, "Expect 'do' after loop conditions.");

    int exitJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP);

    writeIntArray(&current->loops, currentChunk()->count);
    controlBlock();
    emitLoop(loopStart);

    while (current->loopBreaks.count > 0 && current->loopBreaks.array[current->loopBreaks.count - 1] >
           current->loops.array[current->loops.count - 1]) {
        patchJump(current->loopBreaks.array[current->loopBreaks.count - 1]);
        current->loopBreaks.count--;
    }
    current->loops.count--;

    patchJump(exitJump);
    emitByte(OP_POP);
}

static void synchronize() {
    parser.panicMode = false;

    while (parser.current.type != TOKEN_EOF) {
        if (parser.previous.type == TOKEN_SEMICOLON) return;
        switch (parser.current.type) {
            case TOKEN_CLASS:
            case TOKEN_DEFINE:
            case TOKEN_VAR:
            case TOKEN_FOR:
            case TOKEN_IF:
            case TOKEN_WHILE:
            case TOKEN_PRINT:
            case TOKEN_RETURN:
                return;
            default:
                ; // Do nothing.
        }

        advance();
    }
}

static void declaration() {
    if (match(TOKEN_CLASS)) {
        classDeclaration();
    } else if (match(TOKEN_CONST)) {
        constVarDeclaration();
    } else if (match(TOKEN_DEFINE)) {
        funDeclaration();
    } else if (match(TOKEN_VAR) || match(TOKEN_INT) // Seems slow, should find a better way later
        || match(TOKEN_STR) || match(TOKEN_DOUBLE)
        || match(TOKEN_BOOL) || match(TOKEN_CHAR)
        || match(TOKEN_FUN)) {
        varDeclaration(false, false);
    } else {
        statement();
    }

    if (parser.panicMode) synchronize();
}

static void statement() {
    if (match(TOKEN_PRINT)) {
        printStatement();
    } else if (match(TOKEN_BREAK)) {
        breakStatement();
    } else if (match(TOKEN_CONTINUE)) {
        continueStatement();
    } else if (match(TOKEN_FOR)) {
        forStatement();
    } else if (match(TOKEN_IF)) {
        ifStatement();
    } else if (match(TOKEN_RETURN)) {
        returnStatement();
    } else if (match(TOKEN_SWITCH)) {
        switchStatement();
    } else if (match(TOKEN_WHILE)) {
        whileStatement();
    } else if (match(TOKEN_START_BLOCK)) {
        beginScope();
        block();
        endScope();
    } else {
        expressionStatement();
    }
}

static void integer(bool canAssign) {
    int value = atoi(parser.previous.start);
    emitConstant(INT_VAL(value));
}

static void number(bool canAssign) {
    double value = strtod(parser.previous.start, NULL);
    emitConstant(NUMBER_VAL(value));
}

static void _or(bool canAssign) {
    int endJump = emitJump(OP_JUMP_IF_TRUE);

    emitByte(OP_POP);
    parsePrecedence(PREC_OR);

    patchJump(endJump);
}

static void string(bool canAssign) {
    emitConstant(OBJ_VAL(copyString(parser.previous.start + 1,
                               parser.previous.length - 2))); // Change to allocateSourceString later
}

static void namedVariable(Token name, bool canAssign) {
    uint8_t getOp, setOp;
    bool isConst = false;
    int arg = resolveLocal(current, &name);
    if (arg != -1) {
        getOp = OP_GET_LOCAL;
        setOp = OP_SET_LOCAL;
        isConst = current->locals[arg].isConst;
    } else if ((arg = resolveUpvalue(current, &name)) != -1) {
        getOp = OP_GET_UPVALUE;
        setOp = OP_SET_UPVALUE;
    } else {
        arg = identifierConstant(&name);
        getOp = OP_GET_GLOBAL;
        setOp = OP_SET_GLOBAL;
    }

    if (canAssign && match(TOKEN_EQUAL)) {
        if (isConst) {
            error("Cannot re-assign a variable declared as constant.");
            return;
        }
        expression();
        emitBytes(setOp, (uint8_t)arg);
    } else {
        emitBytes(getOp, (uint8_t)arg);
    }
}

static void variable(bool canAssign) {
    namedVariable(parser.previous, canAssign);
}

static void this_(bool canAssign) {
    if (currentClass == NULL) {
        error("Can't use 'this' outside of a class.");
        return;
    }

    variable(false);
}

static void unary(bool canAssign) {
    TokenType operatorType = parser.previous.type;

    // Compile the operand.
    parsePrecedence(PREC_UNARY);

    // Emit the operator instruction.
    switch (operatorType) {
        case TOKEN_NOT: emitByte(OP_NOT); break;
        case TOKEN_MINUS: emitByte(OP_NEGATE); break;
        case TOKEN_TILDE: emitByte(OP_BITWISE_NOT); break;
        default: return; // Unreachable.
    }
}

ParseRule rules[] = {
    [TOKEN_LEFT_PAREN]    = {grouping, call, PREC_CALL},
    [TOKEN_RIGHT_PAREN]   = {NULL, NULL, PREC_NONE},
    [TOKEN_LEFT_BRACE]    = {NULL, NULL, PREC_NONE},
    [TOKEN_RIGHT_BRACE]   = {NULL, NULL, PREC_NONE},
    [TOKEN_START_BLOCK]   = {NULL, NULL, PREC_NONE},
    [TOKEN_END_BLOCK]     = {NULL, NULL, PREC_NONE},
    [TOKEN_COMMA]         = {NULL, NULL, PREC_NONE},
    [TOKEN_DOT]           = {NULL, dot, PREC_CALL},
    [TOKEN_MINUS]         = {unary, binary, PREC_TERM},
    [TOKEN_PLUS]          = {NULL, binary, PREC_TERM},
    [TOKEN_SEMICOLON]     = {NULL, NULL, PREC_NONE},
    [TOKEN_SLASH]         = {NULL, binary, PREC_FACTOR},
    [TOKEN_STAR]          = {NULL, binary, PREC_FACTOR},
    [TOKEN_STAR_STAR]     = {NULL, binary, PREC_POWER},
    [TOKEN_PLUS_EQUAL]    = {NULL, binary, PREC_ASSIGNMENT},
    [TOKEN_MINUS_EQUAL]   = {NULL, binary, PREC_ASSIGNMENT},
    [TOKEN_STAR_EQUAL]    = {NULL, binary, PREC_ASSIGNMENT},
    [TOKEN_SLASH_EQUAL]   = {NULL, binary, PREC_ASSIGNMENT},
    [TOKEN_AMPERSAND]     = {NULL, binary, PREC_BITWISE_AND},
    [TOKEN_VERTICAL_LINE] = {NULL, binary, PREC_BITWISE_OR},
    [TOKEN_CARET]         = {NULL, binary, PREC_BITWISE_XOR},
    [TOKEN_TILDE]         = {unary, NULL, PREC_UNARY},
    [TOKEN_NOT]           = {NULL, NULL, PREC_NONE},
    [TOKEN_BANG_EQUAL]    = {NULL, binary, PREC_EQUALITY},
    [TOKEN_EQUAL]         = {NULL, NULL, PREC_NONE},
    [TOKEN_EQUAL_EQUAL]   = {NULL, binary, PREC_EQUALITY},
    [TOKEN_GREATER]       = {NULL, binary, PREC_COMPARISON},
    [TOKEN_GREATER_EQUAL] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_LESS]          = {NULL, binary, PREC_COMPARISON},
    [TOKEN_LESS_EQUAL]    = {NULL, binary, PREC_COMPARISON},
    [TOKEN_IDENTIFIER]    = {variable, NULL, PREC_NONE},
    [TOKEN_STRING]        = {string, NULL, PREC_NONE},
    [TOKEN_INTEGER]       = {integer, NULL, PREC_NONE},
    [TOKEN_NUMBER]        = {number, NULL, PREC_NONE},
    [TOKEN_AND]           = {NULL, _and, PREC_AND},
    [TOKEN_AS]            = {NULL, NULL, PREC_NONE},
    [TOKEN_BOOL]          = {NULL, NULL, PREC_NONE},
    [TOKEN_BREAK]         = {NULL, NULL, PREC_NONE},
    [TOKEN_CASE]          = {NULL, NULL, PREC_NONE},
    [TOKEN_CONTINUE]      = {NULL, NULL, PREC_NONE},
    [TOKEN_CHAR]          = {NULL, NULL, PREC_NONE},
    [TOKEN_CLASS]         = {NULL, NULL, PREC_NONE},
    [TOKEN_CONST]         = {NULL, NULL, PREC_NONE},
    [TOKEN_DO]            = {NULL, NULL, PREC_NONE},
    [TOKEN_DEFINE]        = {NULL, NULL, PREC_NONE},
    [TOKEN_DEFAULT]       = {NULL, NULL, PREC_NONE},
    [TOKEN_DOUBLE]        = {NULL, NULL, PREC_NONE},
    [TOKEN_ELSE]          = {NULL, NULL, PREC_NONE},
    [TOKEN_END]           = {NULL, NULL, PREC_NONE},
    [TOKEN_FALSE]         = {literal, NULL, PREC_NONE},
    [TOKEN_FUN]           = {NULL, NULL, PREC_NONE},
    [TOKEN_FOR]           = {NULL, NULL, PREC_NONE},
    [TOKEN_IF]            = {NULL, NULL, PREC_NONE},
    [TOKEN_INT]           = {NULL, NULL, PREC_NONE},
    [TOKEN_NIL]           = {literal, NULL, PREC_NONE},
    [TOKEN_NOT]           = {unary, NULL, PREC_NONE},
    [TOKEN_OR]            = {NULL, _or, PREC_OR},
    [TOKEN_PRINT]         = {NULL, NULL, PREC_NONE},
    [TOKEN_RETURN]        = {NULL, NULL, PREC_NONE},
    [TOKEN_SUPER]         = {NULL, NULL, PREC_NONE},
    [TOKEN_STR]           = {NULL, NULL, PREC_NONE},
    [TOKEN_SWITCH]        = {NULL, NULL, PREC_NONE},
    [TOKEN_THEN]          = {NULL, NULL, PREC_NONE},
    [TOKEN_THIS]          = {this_, NULL, PREC_NONE},
    [TOKEN_TRUE]          = {literal, NULL, PREC_NONE},
    [TOKEN_VAR]           = {NULL, NULL, PREC_NONE},
    [TOKEN_WHILE]         = {NULL, NULL, PREC_NONE},
    [TOKEN_ERROR]         = {NULL, NULL, PREC_NONE},
    [TOKEN_EOF]           = {NULL, NULL, PREC_NONE},
};

static void parsePrecedence(Precedence precedence) {
    advance();
    ParseFn prefixRule = getRule(parser.previous.type)->prefix;
    if (prefixRule == NULL) {
        error("Expect expression.");
        return;
    }

    bool canAssign = precedence <= PREC_ASSIGNMENT;
    prefixRule(canAssign);

    while (precedence <= getRule(parser.current.type)->precedence) {
        advance();
        ParseFn infixRule = getRule(parser.previous.type)->infix;
        infixRule(canAssign);
    }

    if (canAssign && match(TOKEN_EQUAL)) {
        error("Invalid assignment target.");
    }
}

static ParseRule* getRule(TokenType type) {
    return &rules[type];
}

ObjFunction* compile(const char* source) {
    initScanner(source);
    Compiler compiler;
    initCompiler(&compiler, TYPE_SCRIPT);

    parser.hadError = false;
    parser.panicMode = false;

    advance();

    while (!match(TOKEN_EOF)) {
        declaration();
    }

    ObjFunction* function = endCompiler();
    return parser.hadError ? NULL : function;
}

void markCompilerRoots() {
    Compiler* compiler = current;
    while (compiler != NULL) {
        markObject((Obj*)compiler->function);
        compiler = compiler->enclosing;
    }
}
