#ifndef compiler_h
#define compiler_h

#include "vm.h"
#include "object.h"

bool compile(const char *source, Chunk *chunk);

#endif
