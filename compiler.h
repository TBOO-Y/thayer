#ifndef thayer_compiler_h
#define thayer_compiler_h

#include "vm.h"

ObjFunction* compile(const char* source);
void markCompilerRoots();

#endif
