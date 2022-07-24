#include <stdio.h>
#include "fnv.h"


struct BytesAndSize {
    char bytes[8];
    size_t size;
};




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



const long NUMERIC_VALUE = 7000;
char STRING_VALUE[] = "blarg";


int main() {


    printf("Hash:\n");


    /* length does not include trailing NUL byte in the test vector */

//    int length_of_buf = sizeof(buf) - 1;
//    Fnv32_t hash_val = fnv_32a_buf(buf, length_of_buf, FNV1_32A_INIT);
    Fnv32_t hash_val1 = fnv_32a_str(STRING_VALUE, FNV1_32A_INIT);


	Fnv32_t bmask = (Fnv32_t) 0xffffffff;

    print_fnv32(hash_val1, bmask, 0, "This is a string test");


    struct BytesAndSize myOutput = convertToBytes(NUMERIC_VALUE);
    printf("Chunks for %ld: %ld\n", NUMERIC_VALUE, myOutput.size);
    printChunks(myOutput);

    Fnv32_t hash_val2 = fnv_32a_buf(myOutput.bytes, myOutput.size, FNV1_32A_INIT);
    print_fnv32(hash_val2, bmask, 0, "This is a numeric test");

    return 0;
}


