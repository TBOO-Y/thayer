#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "object.h"
#include "vm.h"
#include "common.h"
#include "compiler.h"
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
    TokenType type; // I think this is actually useless? Change tomorrow
    bool isConst;
} Local;

typedef struct {
    Local locals[UINT8_COUNT];
    int localCount;
    int scopeDepth;
} Compiler;

Parser parser;
Compiler* current = NULL; // Change later for adaptability to multi-threaded applications, see Chapter 22
Chunk* compilingChunk;

static Chunk* currentChunk() {
    return compilingChunk;
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

static void emitReturn() {
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

static void initCompiler(Compiler* compiler) {
    compiler->localCount = 0;
    compiler->scopeDepth = 0;
    current = compiler;
}

static void endCompiler() {
    emitReturn();
#ifdef DEBUG_PRINT_CODE
    if (!parser.hadError) {
        disassembleChunk(currentChunk(), "code");
    }
#endif
}

static void beginScope() {
    current->scopeDepth++;
}

static void endScope() {
    current->scopeDepth--;
    uint8_t numberPopped = 0;
    while (current->localCount > 0 &&
           current->locals[current->localCount - 1].depth > current->scopeDepth) {
        numberPopped++;
        current->localCount--;
    }
    emitBytes(OP_POPN, numberPopped);
}

static void expression();
static void statement();
static void declaration();
static ParseRule* getRule(TokenType type);
static void parsePrecedence(Precedence precedence);

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

static void addLocal(Token name, TokenType type, bool isConst) {
    if (current->localCount == UINT8_COUNT) {
        error("Too many local variables in function.");
        return;
    }

    Local* local = &current->locals[current->localCount++];
    local->name = name;
    local->depth = -1;
    local->type = type;
    local->isConst = isConst;
}

static void declareVariable(TokenType type, bool isConst) {
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

    addLocal(*name, type, isConst);
}

static uint8_t parseVariable(const char* errorMessage, TokenType type, bool isConst) {
    consume(TOKEN_IDENTIFIER, errorMessage);

    declareVariable(type, isConst);
    if (current->scopeDepth > 0) return 0;

    return identifierConstant(&parser.previous);
}

static void markInitialized(TokenType type, bool isConst) {
    current->locals[current->localCount - 1].depth = current->scopeDepth;
    emitBytes(OP_DEFINE_LOCAL, type);
}

static void defineVariable(uint8_t global, TokenType type, bool isConst) {
    if (current->scopeDepth > 0) {
        markInitialized(type, isConst);
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

static void varDeclaration(bool isConst) {
    TokenType type = parser.previous.type;

    uint8_t global = parseVariable("Expect variable name.", type, isConst);

    if (match(TOKEN_EQUAL)) {
        expression();
    } else {
        emitByte(OP_NIL);
    }
    consume(TOKEN_SEMICOLON, "Expect ; after variable declaration.");

    defineVariable(global, type, isConst);
}

static void constVarDeclaration() {
    if (match(TOKEN_VAR) || match(TOKEN_INT) // Seems slow, should find a better way later
        || match(TOKEN_STR) || match(TOKEN_DOUBLE)
        || match(TOKEN_BOOL) || match(TOKEN_CHAR)) {
        varDeclaration(true);
    } else {
        errorAtCurrent("Type annotation must follow 'const' keyword.");
    }
}

static void expressionStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ; after expression.");
    emitByte(OP_POP);
}

static void printStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ; after value.");
    emitByte(OP_PRINT);
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
    if (match(TOKEN_CONST)) {
        constVarDeclaration();
    } else if (match(TOKEN_VAR) || match(TOKEN_INT) // Seems slow, should find a better way later
        || match(TOKEN_STR) || match(TOKEN_DOUBLE)
        || match(TOKEN_BOOL) || match(TOKEN_CHAR)) {
        varDeclaration(false);
    } else {
        statement();
    }

    if (parser.panicMode) synchronize();
}

static void statement() {
    if (match(TOKEN_PRINT)) {
        printStatement();
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

static void string(bool canAssign) {
    emitConstant(OBJ_VAL(copyString(parser.previous.start + 1,
                               parser.previous.length - 2))); // Change to allocateSourceString later
}

static void namedVariable(Token name, bool canAssign) {
    uint8_t getOp, setOp;
    bool isConst;
    int arg = resolveLocal(current, &name);
    if (arg != -1) {
        getOp = OP_GET_LOCAL;
        setOp = OP_SET_LOCAL;
        isConst = current->locals[arg].isConst;
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
    [TOKEN_LEFT_PAREN]    = {grouping, NULL, PREC_NONE},
    [TOKEN_RIGHT_PAREN]   = {NULL, NULL, PREC_NONE},
    [TOKEN_LEFT_BRACE]    = {NULL, NULL, PREC_NONE},
    [TOKEN_RIGHT_BRACE]   = {NULL, NULL, PREC_NONE},
    [TOKEN_START_BLOCK]   = {NULL, NULL, PREC_NONE},
    [TOKEN_END_BLOCK]     = {NULL, NULL, PREC_NONE},
    [TOKEN_COMMA]         = {NULL, NULL, PREC_NONE},
    [TOKEN_DOT]           = {NULL, NULL, PREC_NONE},
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
    [TOKEN_AND]           = {NULL, NULL, PREC_AND},
    [TOKEN_AS]            = {NULL, NULL, PREC_NONE},
    [TOKEN_BOOL]          = {NULL, NULL, PREC_NONE},
    [TOKEN_CHAR]          = {NULL, NULL, PREC_NONE},
    [TOKEN_CLASS]         = {NULL, NULL, PREC_NONE},
    [TOKEN_DO]            = {NULL, NULL, PREC_NONE},
    [TOKEN_DEFINE]        = {NULL, NULL, PREC_NONE},
    [TOKEN_DOUBLE]        = {NULL, NULL, PREC_NONE},
    [TOKEN_ELSE]          = {NULL, NULL, PREC_NONE},
    [TOKEN_END]           = {NULL, NULL, PREC_NONE},
    [TOKEN_FALSE]         = {literal, NULL, PREC_NONE},
    [TOKEN_FOR]           = {NULL, NULL, PREC_NONE},
    [TOKEN_IF]            = {NULL, NULL, PREC_NONE},
    [TOKEN_INT]           = {NULL, NULL, PREC_NONE},
    [TOKEN_NIL]           = {literal, NULL, PREC_NONE},
    [TOKEN_NOT]           = {unary, NULL, PREC_NONE},
    [TOKEN_OR]            = {NULL, NULL, PREC_NONE},
    [TOKEN_PRINT]         = {NULL, NULL, PREC_NONE},
    [TOKEN_RETURN]        = {NULL, NULL, PREC_NONE},
    [TOKEN_SUPER]         = {NULL, NULL, PREC_NONE},
    [TOKEN_STR]           = {NULL, NULL, PREC_NONE},
    [TOKEN_THIS]          = {NULL, NULL, PREC_NONE},
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

bool compile(const char* source, Chunk* chunk) {
    initScanner(source);
    Compiler compiler;
    initCompiler(&compiler);
    compilingChunk = chunk;

    parser.hadError = false;
    parser.panicMode = false;

    advance();

    while (!match(TOKEN_EOF)) {
        declaration();
    }

    consume(TOKEN_EOF, "Expect end of expression.");
    endCompiler();
    return !parser.hadError;
}
