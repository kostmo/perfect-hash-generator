#!/bin/bash -xe


GENERATED_DIR="${1:-local-gen}"
GENERATED_CODE_DIR=$GENERATED_DIR/code

EXEC_SUFFIX="${2:-int}"

OUTPUT_FILENAME=demo-$EXEC_SUFFIX
FNV_LIB_DIR=fnv

FILES="demo.c utils.c lookup.c $GENERATED_CODE_DIR/generated_lookup.c $GENERATED_CODE_DIR/generated_values.c"

FNV_LIB_NAME=fnv
FNV_LIB_ARCHIVE_FILENAME=lib$FNV_LIB_NAME.a

make -C $FNV_LIB_DIR $FNV_LIB_ARCHIVE_FILENAME

gcc -Wall -o $OUTPUT_FILENAME -I$FNV_LIB_DIR -I$GENERATED_CODE_DIR -L$FNV_LIB_DIR $FILES -l$FNV_LIB_NAME

./$OUTPUT_FILENAME
