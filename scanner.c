#include <stdio.h>
#include <string.h>

#include "common.h"
#include "scanner.h"

#include <stdlib.h>

typedef struct {
    const char* start;
    const char* current;
    int line;
} Scanner;

Scanner scanner;

void initScanner(const char* source) {
    scanner.start = source;
    scanner.current = source;
    scanner.line = 1;
}

static bool isAlpha(char c) {
    return (c >= 'a' && c <= 'z') ||
           (c >= 'A' && c <= 'Z') ||
            c == '_';
}

static bool isDigit(char c) {
    return c >= '0' && c <= '9';
}

bool isAtEnd() {
    return *scanner.current == '\0';
}

static char advance() {
    scanner.current++;
    return scanner.current[-1];
}

static char peek() {
    return *scanner.current;
}

static char peekNext() {
    if (isAtEnd()) return '\0';
    return scanner.current[1];
}

static bool match(char expected) {
    if (isAtEnd()) return false;
    if (*scanner.current != expected) return false;
    scanner.current++;
    return true;
}

static Token makeToken(TokenType type) {
    Token token;
    token.type = type;
    token.start = scanner.start;
    token.length = (int)(scanner.current - scanner.start);
    token.line = scanner.line;
    return token;
}

static Token errorToken(const char* message) {
    Token token;
    token.type = TOKEN_ERROR;
    token.start = message;
    token.length = (int)strlen(message);
    token.line = scanner.line;
    return token;
}

static void skipWhitespace() {
    for (;;) {
        char c = peek();
        switch (c) {
            case ' ': // Will have a lot more for this part later
            case '\r':
            case '\t':
                advance();
                break;
            case '\n':
                scanner.line++;
                advance();
                break;
            case '/':
                if (peekNext() == '/') {
                    // A comment goes until the end of the line.
                    while (peek() != '\n' && !isAtEnd()) advance();
                } else {
                    return;
                }
                break;
            default:
                return;
        }
    }
}

static TokenType checkKeyword(int start, int length, const char* rest, TokenType type) {
    if (scanner.current - scanner.start == start + length &&
        memcmp(scanner.start + start, rest, length) == 0) {
        return type;
    }

    return TOKEN_IDENTIFIER;
}

static TokenType identifierType() {
    switch (scanner.start[0]) {
        case 'a':
            if (scanner.current - scanner.start > 1) {
                switch (scanner.start[1]) {
                    case 's': return scanner.current - scanner.start == 2 ? TOKEN_AS : TOKEN_IDENTIFIER;
                    case 'n':
                        if (scanner.current - scanner.start == 3) {
                            switch (scanner.start[2]) {
                                case 'd': return TOKEN_AND;
                                default: return TOKEN_IDENTIFIER;
                            }
                        }
                }
            }
            break;
        case 'b':
            if (scanner.current - scanner.start > 1) {
                switch (scanner.start[1]) {
                    case 'o': return checkKeyword(2, 2, "ol", TOKEN_BOOL);
                    case 'r': return checkKeyword(2, 3, "eak", TOKEN_BREAK);
                }
            }
        case 'c':
            if (scanner.current - scanner.start > 1) {
                switch (scanner.start[1]) {
                    case 'h': return checkKeyword(2, 2, "ar", TOKEN_CHAR);
                    case 'l': return checkKeyword(2, 3, "ass", TOKEN_CLASS);
                    case 'o': {
                        TokenType type = checkKeyword(2, 3, "nst", TOKEN_CONST);
                        if (type == TOKEN_CONST) {
                            return type;
                        }
                        return checkKeyword(2, 6, "ntinue", TOKEN_CONTINUE);
                    }
                }
            }
            break;
        case 'd':
            if (scanner.current - scanner.start > 1) {
                switch (scanner.start[1]) {
                    case 'e': {
                        TokenType type = checkKeyword(2, 4, "fine", TOKEN_DEFINE);
                        if (type == TOKEN_DEFINE) {
                            return type;
                        }
                        return checkKeyword(2, 5, "fault", TOKEN_DEFAULT);
                    }
                    case 'o': return checkKeyword(2, 4, "uble", TOKEN_DOUBLE);
                }
            }
            break;
        case 'e':
            if (scanner.current - scanner.start > 1) {
                switch (scanner.start[1]) {
                    case 'l': return checkKeyword(2, 2, "se", TOKEN_ELSE);
                    case 'n': return checkKeyword(2, 1, "d", TOKEN_END);
                    case 'b': return checkKeyword(2, 4, "lock", TOKEN_END_BLOCK);
                }
            }
            break;
        case 'f':
            if (scanner.current - scanner.start > 1) {
                switch (scanner.start[1]) {
                    case 'a': return checkKeyword(2, 3, "lse", TOKEN_FALSE);
                    case 'o': return checkKeyword(2, 1, "r", TOKEN_FOR);
                }
            }
            break;
        case 'i':
            if (scanner.current - scanner.start > 1) {
                switch (scanner.start[1]) {
                    case 'f': return scanner.current - scanner.start == 2 ? TOKEN_IF : TOKEN_IDENTIFIER;
                    case 'n': return checkKeyword(2, 1, "t", TOKEN_INT);
                }
            }
            break;
        case 'n':
            if (scanner.current - scanner.start > 1) {
                switch (scanner.start[1]) {
                    case 'i': return checkKeyword(2, 1, "l", TOKEN_NIL);
                    case 'o': return checkKeyword(2, 1, "t", TOKEN_NOT);
                }
            }
            break;
        case 'o': return checkKeyword(1, 1, "r", TOKEN_OR);
        case 'p': return checkKeyword(1, 4, "rint", TOKEN_PRINT);
        case 'r': return checkKeyword(1, 5, "eturn", TOKEN_RETURN);
        case 's':
            if (scanner.current - scanner.start > 1) {
                switch (scanner.start[1]) {
                    case 'u': return checkKeyword(2, 3, "per", TOKEN_SUPER);
                    case 't': return checkKeyword(2, 1, "r", TOKEN_STR);
                    case 'b': return checkKeyword(2, 4, "lock", TOKEN_START_BLOCK);
                    case 'w': return checkKeyword(2, 4, "itch", TOKEN_SWITCH);
                }
            }
        case 't':
            if (scanner.current - scanner.start > 1) {
                switch (scanner.start[1]) {
                    case 'h':
                        if (scanner.current - scanner.start > 2) {
                            switch (scanner.start[2]) {
                                case 'e': return checkKeyword(3, 1, "n", TOKEN_THEN);
                                case 'i': return checkKeyword(3, 1, "s", TOKEN_THIS);
                                default: return TOKEN_IDENTIFIER;
                            }
                        }
                        return TOKEN_IDENTIFIER;
                    case 'r': return checkKeyword(2, 2, "ue", TOKEN_TRUE);
                }
            }
            break;
        case 'v': return checkKeyword(1, 2, "ar", TOKEN_VAR);
        case 'w': return checkKeyword(1, 2, "hile", TOKEN_WHILE);
    }

    return TOKEN_IDENTIFIER;
}

static Token identifier() {
    while (isAlpha(peek()) || isDigit(peek())) advance();
    return makeToken(identifierType());
}

static Token number() {
    bool hasDecimal = false;
    if (isDigit(peek())) advance();

    // Look for a fractional part.
    if (peek() == '.' && isDigit(peekNext())) {
        hasDecimal = true;
        // Consume the ".".
        advance();

        while (isDigit(peek())) advance();
    }

    return hasDecimal ? makeToken(TOKEN_NUMBER) : makeToken(TOKEN_INTEGER);
}

static Token string() { // Implement string interpolation later
    while (peek() != '"' && !isAtEnd()) {
        if (peek() == '\n') scanner.line++;
        advance();
    }

    if (isAtEnd()) return errorToken("Unterminated string.");

    // The closing quote.
    advance();
    return makeToken(TOKEN_STRING);
}

Token scanToken() {
    skipWhitespace();
    scanner.start = scanner.current;

    if (isAtEnd()) return makeToken(TOKEN_EOF);

    char c = advance();
    if (isAlpha(c)) return identifier();
    if (isDigit(c)) return number();

    switch (c) {
        case '(': return makeToken(TOKEN_LEFT_PAREN);
        case ')': return makeToken(TOKEN_RIGHT_PAREN);
        case '{': return makeToken(TOKEN_LEFT_BRACE);
        case '}': return makeToken(TOKEN_RIGHT_BRACE);
        case ':': return makeToken(TOKEN_COLON);
        case ';': return makeToken(TOKEN_SEMICOLON);
        case ',': return makeToken(TOKEN_COMMA);
        case '.': return makeToken(TOKEN_DOT);
        case '-': return makeToken(match('=') ? TOKEN_MINUS_EQUAL : TOKEN_MINUS);
        case '+': return makeToken(match('=') ? TOKEN_PLUS_EQUAL : TOKEN_PLUS);
        case '/': return makeToken(match('=') ? TOKEN_SLASH_EQUAL : TOKEN_SLASH);
        case '*': return makeToken(match('*') ? TOKEN_STAR_STAR :
                                  (match('=') ? TOKEN_STAR_EQUAL : TOKEN_STAR));
        case '&': return makeToken(TOKEN_AMPERSAND);
        case '|': return makeToken(TOKEN_VERTICAL_LINE);
        case '^': return makeToken(TOKEN_CARET);
        case '~': return makeToken(TOKEN_TILDE);
        case '=': return makeToken(match('=') ? TOKEN_EQUAL_EQUAL : TOKEN_EQUAL);
        case '<': return makeToken(match('=') ? TOKEN_LESS_EQUAL : TOKEN_LESS);
        case '>': return makeToken(match('=') ? TOKEN_GREATER_EQUAL : TOKEN_GREATER);
        case '"': return string();
    }

    return errorToken("Unexpected character.");
}
