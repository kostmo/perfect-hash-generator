#!/bin/bash -xe


OUTPUT_FILENAME=demo
FNV_LIB_DIR=fnv
GENERATED_CODE_DIR=lookup_gen

FILES="demo.c utils.c lookup.c $GENERATED_CODE_DIR/generated_lookup.c $GENERATED_CODE_DIR/generated_values.c"


make -C $FNV_LIB_DIR libfnv.a

gcc -Wall -o $OUTPUT_FILENAME -I$FNV_LIB_DIR -I$GENERATED_CODE_DIR -L$FNV_LIB_DIR $FILES -lfnv

./$OUTPUT_FILENAME
