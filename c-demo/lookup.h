#pragma once

#include <stdio.h>
#include "fnv.h"

union IntermediateVal {
   int i;
   Fnv32_t hash;
};


struct LookupTable {
    size_t size;
    int *values;
    union IntermediateVal *redirects;
};


int lookup(struct LookupTable table, int key);
