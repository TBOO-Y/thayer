#ifndef thayer_value_h
#define thayer_value_h

#include "common.h"

typedef struct Obj Obj;
typedef struct ObjString ObjString;

typedef enum {
    VAL_BOOL,
    VAL_CHAR,
    VAL_INT,
    VAL_NIL,
    VAL_NUMBER,
    VAL_OBJ,
    VAL_ERR
} ValueType;

typedef struct {
    ValueType type;
    union {
        bool boolean;
        char chara;
        int integer;
        double number;
        Obj* obj;
    } as;
} Value;

#define IS_BOOL(value)      ((value).type == VAL_BOOL)
#define IS_CHAR(value)      ((value).type == VAL_CHAR)
#define IS_ERR(value)       ((value).type == VAL_ERR)
#define IS_INT(value)       ((value).type == VAL_INT)
#define IS_NIL(value)       ((value).type == VAL_NIL)
#define IS_NUMBER(value)    ((value).type == VAL_NUMBER)
#define IS_NUMERICAL(value) (IS_NUMBER(value) || IS_INT(value) || IS_CHAR(value))
#define IS_OBJ(value)       ((value).type == VAL_OBJ)

#define AS_OBJ(value)       ((value).as.obj)
#define AS_BOOL(value)      ((value).as.boolean)
#define AS_CHAR(value)      ((value).as.chara)
#define AS_INT(value)       ((value).as.integer)
#define AS_NUMBER(value)    ((value).as.number)

#define BOOL_VAL(value)     ((Value){VAL_BOOL, {.boolean = value}})
#define CHAR_VAL(value)     ((Value){VAL_CHAR, {.chara = value}})
#define ERR_VAL             ((Value){VAL_ERR, {.number = 1}})
#define INT_VAL(value)      ((Value){VAL_INT, {.integer = value}})
#define NIL_VAL             ((Value){VAL_NIL, {.number = 0}})
#define NUMBER_VAL(value)   ((Value){VAL_NUMBER, {.number = value}})
#define OBJ_VAL(object)     ((Value){VAL_OBJ, {.obj = (Obj*)object}})

typedef struct {
    int capacity;
    int count;
    Value* values;
} ValueArray;

bool valuesEqual(Value a, Value b);
void initValueArray(ValueArray* array);
void writeValueArray(ValueArray* array, Value value);
void freeValueArray(ValueArray* array);
void printValue(Value value);

#endif
