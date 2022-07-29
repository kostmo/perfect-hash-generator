#include <stdio.h>
#include "fnv.h"
#include "utils.h"


const long NUMERIC_VALUE = 7000;
char STRING_VALUE[] = "blarg";



const Fnv32_t bmask = (Fnv32_t) 0xffffffff;


int main(int argc, char *argv[]) {

    /* length does not include trailing NUL byte in the test vector */

//    int length_of_buf = sizeof(buf) - 1;
//    Fnv32_t hash_val = fnv_32a_buf(buf, length_of_buf, FNV1_32A_INIT);
    Fnv32_t hash_val1 = fnv_32a_str(STRING_VALUE, FNV1_32A_INIT);


    printf("KARL: argc: %d\n", argc);
    printf("KARL: argv[0]: %s\n", argv[0]);
    printf("KARL: argv[1]: %s\n", argv[1]);
    printf("KARL: argv[2]: %s\n", argv[2]);


    print_fnv32(hash_val1, bmask, 0, "This is a string test");


    LongNumberBuffer myOutput = convertToBytes(NUMERIC_VALUE);
    printf("Chunks for %ld: %ld\n", NUMERIC_VALUE, myOutput.size);
    printChunks(myOutput);

    Fnv32_t hash_val2 = fnv_32a_buf(myOutput.bytes, myOutput.size, FNV1_32A_INIT);
    print_fnv32(hash_val2, bmask, 0, "This is a numeric test");


    if (argv[1][0] == 'i') {
        printf("============== COMPARING INTS =============\n");
        bool ints_are_correct = verify_int_key_lookup_correctness(argv[2]);
        return !ints_are_correct;
    } else {
        printf("============== COMPARING STRINGS =============\n");
        bool strings_are_correct = verify_string_key_lookup_correctness(argv[2]);
        return !strings_are_correct;
    }
}


