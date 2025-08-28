#include <stdio.h>

#include "type_checker.h"
#include "object.h"

bool typeCheck(Value value, TokenType type) {
    switch (type) {
        case TOKEN_BOOL:
            return IS_BOOL(value);
        case TOKEN_CHAR:
            return IS_CHAR(value);
        case TOKEN_DOUBLE:
            return IS_NUMBER(value);
        case TOKEN_INT:
            return IS_INT(value);
        case TOKEN_STR: // Should be changed later to accommodate more types of objects
            return IS_STRING(value);
        case TOKEN_FUN:
            return IS_CLOSURE(value) || IS_FUNCTION(value);
        case TOKEN_CLASS:
            return IS_CLASS(value);
        default: return false; // Unreachable.
    }
}

ValueType tagToType(TokenType type) {
    switch (type) {
        case TOKEN_BOOL:   return VAL_BOOL;
        case TOKEN_CHAR:   return VAL_CHAR;
        case TOKEN_DOUBLE: return VAL_NUMBER;
        case TOKEN_INT:    return VAL_INT;
        case TOKEN_FUN:    return VAL_OBJ;
        case TOKEN_CLASS:  return VAL_OBJ;
        case TOKEN_STR:    return VAL_OBJ; // Should be changed later to accommodate more types of objects
        default:           return VAL_NIL;
    }
}

const char* typeToName(TokenType type) {
    switch (type) {
        case TOKEN_BOOL:   return "bool";
        case TOKEN_CHAR:   return "char";
        case TOKEN_DOUBLE: return "double";
        case TOKEN_INT:    return "int";
        case TOKEN_STR:    return "string";
        case TOKEN_FUN:    return "function";
        case TOKEN_CLASS:  return "class";
        default:           return "nil";
    }
}

const char* getValueTypeName(Value value) {
    switch (value.type) {
        case VAL_BOOL:     return "bool";
        case VAL_CHAR:     return "char";
        case VAL_NUMBER:   return "double";
        case VAL_INT:      return "int";
        case VAL_OBJ: {
            switch (value.as.obj->type) {
                case OBJ_CLOSURE:  return "closure";
                case OBJ_CLASS:    return "class";
                case OBJ_FUNCTION: return "function";
                case OBJ_INSTANCE: return "instance";
                case OBJ_NATIVE:   return "native function";
                case OBJ_STRING:   return "string";
                default:           return "nil";
            }
        }
        default:           return "nil";
    }
}

void printType(TokenType type) {
    printf("%s", typeToName(type));
}
