#include "utils.h"


Fnv32_t fnv_32a_numeric_buf(LongNumberBuffer buf, Fnv32_t initial_basis) {
    return fnv_32a_buf(buf.bytes, buf.size, initial_basis);
}

int countRequiredBytes(long num) {

    int count = 0;
    while (num) {
        count++;
        num >>= 8;
    }

    return count;

}

LongNumberBuffer convertToBytes(long num) {

    int count = countRequiredBytes(num);

    LongNumberBuffer myOutput = { .bytes = {0}, .size = count };

    for (int i = count - 1; i >= 0; i--) {

        char chunk = num & 0xff;
        myOutput.bytes[i] = chunk;

        num >>= 8;
    }

    return myOutput;
}

void printChunks(LongNumberBuffer chunks) {

    for (int i=0; i<chunks.size; i++) {

        printf("%02x", chunks.bytes[i]);
    }
    printf("\n");
}

