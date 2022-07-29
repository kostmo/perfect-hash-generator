#include "lookup.h"
#include "generated_lookup.h"


unsigned int lookup(LongNumberBuffer key) {

    // Step 1:
    Fnv32_t hash_val = fnv_32a_numeric_buf(key, FNV1_32A_INIT);

    unsigned int redirector_position = hash_val % MY_SIZE;
//    printf("redirector_position: %d\n", redirector_position);

    // Step 2:
    Fnv32_t redirect_val = MY_NONCES[redirector_position];
    if (((int32_t) redirect_val) < 0) {
        // Direct lookup
        return -(((int32_t) redirect_val) + 1);
    } else {
        Fnv32_t next_hash_val = fnv_32a_numeric_buf(key, redirect_val);
//        Fnv32_t bmask = (Fnv32_t) 0xffffffff;
//        print_fnv32(next_hash_val, bmask, 0, "This is a numeric test");
        return next_hash_val % MY_SIZE;
    }
}





unsigned int lookup_str(char* key) {

    // Step 1:
    Fnv32_t hash_val = fnv_32a_str(key, FNV1_32A_INIT);

    unsigned int redirector_position = hash_val % MY_SIZE;
//    printf("redirector_position: %d\n", redirector_position);

    // Step 2:
    Fnv32_t redirect_val = MY_NONCES[redirector_position];
    if (((int32_t) redirect_val) < 0) {
        // Direct lookup
        return -(((int32_t) redirect_val) + 1);
    } else {
        Fnv32_t next_hash_val = fnv_32a_str(key, redirect_val);
//        Fnv32_t bmask = (Fnv32_t) 0xffffffff;
//        print_fnv32(next_hash_val, bmask, 0, "This is a numeric test");
        return next_hash_val % MY_SIZE;
    }
}
