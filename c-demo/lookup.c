#include "lookup.h"


unsigned int lookup(LookupTable table, LongNumberBuffer key) {

	Fnv32_t bmask = (Fnv32_t) 0xffffffff;


    // Step 1:
    Fnv32_t hash_val = fnv_32a_numeric_buf(key, FNV1_32A_INIT);

    printf("-----------\n");
    printf("table.size: %d\n", table.size);
    printf("First hash val:\n");
    print_fnv32(hash_val, bmask, 0, "This is a numeric test");
    printf("-----------\n");

    unsigned int redirector_position = hash_val % table.size;

    printf("redirector_position: %d\n", redirector_position);
    printf("-----------\n");

    NonceValPair table_val = table.elems[redirector_position];

    union IntermediateVal redirect_val = table_val.nonce;

    unsigned int value_position;
    if (redirect_val.signed_val < 0) {

        printf("Case 1\n");

        value_position = -(redirect_val.signed_val + 1);
    } else {

        printf("Case 2\n");

        Fnv32_t next_hash_val = fnv_32a_numeric_buf(key, redirect_val.nonce);

        print_fnv32(next_hash_val, bmask, 0, "This is a numeric test");


        value_position = next_hash_val % table.size;
    }

    return value_position;
}
