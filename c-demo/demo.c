#include <stdio.h>
#include <string.h>

#include "fnv.h"
#include "utils.h"


const long NUMERIC_VALUE = 7000;
const Fnv32_t bmask = (Fnv32_t) 0xffffffff;


int main(int argc, char *argv[]) {

    LongNumberBuffer myOutput = convertToBytes(NUMERIC_VALUE);
    printf("Chunks for %ld: %ld\n", NUMERIC_VALUE, myOutput.size);
    printChunks(myOutput);

    Fnv32_t hash_val2 = fnv_32a_buf(myOutput.bytes, myOutput.size, FNV1_32A_INIT);
    print_fnv32(hash_val2, bmask, 0, "This is a numeric test");

    if (strcmp("int", argv[1]) == 0) {
        printf("============== COMPARING INTS =============\n");
        bool ints_are_correct = verify_int_key_lookup_correctness(argv[2]);
        return !ints_are_correct;
    } else {
        printf("============== COMPARING STRINGS =============\n");
        bool strings_are_correct = verify_string_key_lookup_correctness(argv[2]);
        return !strings_are_correct;
    }
}


