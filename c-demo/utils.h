#pragma once

#include <stdio.h>
#include "fnv.h"


typedef struct LongNumberBuffer {
    char bytes[8];
    size_t size;
} LongNumberBuffer;

Fnv32_t fnv_32a_numeric_buf(LongNumberBuffer, Fnv32_t);

int countRequiredBytes(long);

LongNumberBuffer convertToBytes(long);

void printChunks(LongNumberBuffer);
