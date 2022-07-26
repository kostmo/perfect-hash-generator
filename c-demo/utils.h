#pragma once

#include <stdio.h>

struct BytesAndSize {
    char bytes[8];
    size_t size;
};


int countRequiredBytes(long num);

struct BytesAndSize convertToBytes(long num);

void printChunks(struct BytesAndSize chunks);
