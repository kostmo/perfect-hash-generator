#include <stdio.h>
#include "fnv.h"


int main() {


    printf("Hash:\n");


    /* length does not include trailing NUL byte in the test vector */
    char buf[] = "blah";
    int length_of_buf = sizeof(buf) - 1;
    Fnv32_t hash_val = fnv_32a_buf(buf, length_of_buf, FNV1_32A_INIT);


	Fnv32_t bmask = (Fnv32_t)0xffffffff;

    print_fnv32(hash_val, bmask, 0, "foo");

    return 0;
}

