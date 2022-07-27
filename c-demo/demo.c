#include <stdio.h>
#include "fnv.h"
#include "lookup.h"
#include "utils.h"

#include "generated_values.h"


const long NUMERIC_VALUE = 7000;
char STRING_VALUE[] = "blarg";



const Fnv32_t bmask = (Fnv32_t) 0xffffffff;


int main() {

    /* length does not include trailing NUL byte in the test vector */

//    int length_of_buf = sizeof(buf) - 1;
//    Fnv32_t hash_val = fnv_32a_buf(buf, length_of_buf, FNV1_32A_INIT);
    Fnv32_t hash_val1 = fnv_32a_str(STRING_VALUE, FNV1_32A_INIT);



    print_fnv32(hash_val1, bmask, 0, "This is a string test");


    LongNumberBuffer myOutput = convertToBytes(NUMERIC_VALUE);
    printf("Chunks for %ld: %ld\n", NUMERIC_VALUE, myOutput.size);
    printChunks(myOutput);

    Fnv32_t hash_val2 = fnv_32a_buf(myOutput.bytes, myOutput.size, FNV1_32A_INIT);
    print_fnv32(hash_val2, bmask, 0, "This is a numeric test");

    int inputs[] = {73, 21, 98, 11, 39, 40, 85};
    int input_count = sizeof(inputs)/sizeof(inputs[0]);
    printf("input_count: %d\n", input_count);
    for (int i=0; i<input_count; i++) {
        printf("=========================\n");
        printf("Iteration: %d\n", i);

        int value_position = lookup(convertToBytes(inputs[i]));
        printf("Value position: %d\n", value_position);
        printf("Actual value: %d\n", HASHED_VALUES[value_position]);
    }

    return 0;
}


