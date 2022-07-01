#include <stdio.h>
#include "fnv/fnv.h"


int main() {


    printf("Hash:\n");





    Fnv32_t hash_val = fnv_32a_buf(buf, length_of_buf, FNV1_32A_INIT);


	Fnv32_t bmask = (Fnv32_t)0xffffffff;

    print_fnv32(hash_val, bmask, 0, "foo");

    return 0;
}



Fnv32_t compute_hash(char[] data_octets, int octet_count) {




        
    return hash_val;
}
