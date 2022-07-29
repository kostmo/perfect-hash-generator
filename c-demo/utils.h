#pragma once

#include <stdio.h>
#include <stdbool.h>
#include "fnv.h"
#include "generated_values.h"


typedef struct LongNumberBuffer {
    char bytes[8];
    size_t size;
} LongNumberBuffer;

Fnv32_t fnv_32a_numeric_buf(LongNumberBuffer, Fnv32_t);

int countRequiredBytes(long);

LongNumberBuffer convertToBytes(long);

void printChunks(LongNumberBuffer);


void read_int_pairs(int keyArray[], GENERATED_VALUES_TYPE valueArray[], int, const char*);
void read_string_pairs(char* keyArray[], GENERATED_VALUES_TYPE valueArray[], int, const char*);

bool verify_int_key_lookup_correctness();
bool verify_string_key_lookup_correctness();
