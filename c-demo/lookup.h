#pragma once

#include <stdio.h>
#include "fnv.h"
#include "utils.h"

union IntermediateVal {
   int signed_val;
   Fnv32_t nonce;
};


typedef struct NonceValPair {
    union IntermediateVal nonce;
    int value;
} NonceValPair;


typedef struct LookupTable {
    size_t size;
    struct NonceValPair elems[];
} LookupTable;


int lookup(LookupTable, struct LongNumberBuffer);
