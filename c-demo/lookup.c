#include "lookup.h"


int lookup(LookupTable table, LongNumberBuffer key) {

    // Step 1:
    Fnv32_t hash_val = fnv_32a_numeric_buf(key, FNV1_32A_INIT);
    int redirector_position = hash_val % table.size;
    NonceValPair table_val = table.elems[redirector_position];
    union IntermediateVal redirect_val = table_val.nonce;

    int value_position;
    if (redirect_val.signed_val < 0) {
        value_position = -(redirect_val.signed_val + 1);
    } else {
        Fnv32_t next_hash_val = fnv_32a_numeric_buf(key, redirect_val.nonce);
        value_position = next_hash_val % table.size;
    }

    return value_position;
}
