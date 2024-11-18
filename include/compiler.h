#ifndef compiler_h
#define compiler_h

#include "vm.h"
#include "object.h"

ObjFunction *compile(const char *source);

#endif
