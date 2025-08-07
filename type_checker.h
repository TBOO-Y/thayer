#ifndef thayer_type_checker_h
#define thayer_type_checker_h

#include "scanner.h"
#include "value.h"

#define SAME_TYPE(val1, val2)     ((val1).type == (val2).type)
#define SAME_OBJ_TYPE(val1, val2) ((val1).as.obj->type == (val2).as.obj->type)

bool typeCheck(Value value, TokenType type);
ValueType tagToType(TokenType type);
const char* typeToName(TokenType type);
const char* getValueTypeName(Value value);
void printType(TokenType type);

#endif
