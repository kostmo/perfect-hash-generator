#pragma once

#include <stdio.h>
#include <stdbool.h>
#include "fnv.h"


typedef struct LongNumberBuffer {
    char bytes[8];
    size_t size;
} LongNumberBuffer;

Fnv32_t fnv_32a_numeric_buf(LongNumberBuffer, Fnv32_t);

int countRequiredBytes(long);

LongNumberBuffer convertToBytes(long);

void printChunks(LongNumberBuffer);


void read_ints(int keyArray[], int valueArray[], int, const char*);

bool verify_lookup_correctness();
