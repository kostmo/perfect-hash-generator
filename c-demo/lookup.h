#pragma once

#include <stdio.h>
#include "fnv.h"
#include "utils.h"

union IntermediateVal {
   int32_t signed_val;
   Fnv32_t nonce;
};


typedef struct NonceValPair {
    union IntermediateVal nonce;
    int value;
} NonceValPair;


typedef struct LookupTable {
    size_t size;
    NonceValPair elems[7];
} LookupTable;


unsigned int lookup(LookupTable, LongNumberBuffer);


extern const size_t my_size;
extern const NonceValPair my_elems[];

