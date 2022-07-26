#include "utils.h"


int countRequiredBytes(long num) {

    int count = 0;
    while (num) {
        count++;
        num >>= 8;
    }

    return count;

}

struct BytesAndSize convertToBytes(long num) {

    int count = countRequiredBytes(num);

    struct BytesAndSize myOutput = { .bytes = {0}, .size = count };

    for (int i = count - 1; i >= 0; i--) {

        char chunk = num & 0xff;
        myOutput.bytes[i] = chunk;

        num >>= 8;
    }

    return myOutput;
}

void printChunks(struct BytesAndSize chunks) {

    for (int i=0; i<chunks.size; i++) {

        printf("%02x", chunks.bytes[i]);
    }
    printf("\n");
}

