#include <stdio.h>
#include "fnv.h"
#include "lookup.h"
#include "utils.h"




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


