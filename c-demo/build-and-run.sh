#!/bin/bash -xe


cd ../haskell
./demo-c-codegen.sh
cd -



OUTPUT_FILENAME=demo
FNV_LIB_DIRECTORY=fnv

FILES="demo.c utils.c lookup.c lookup_gen/generated_lookup.c lookup_gen/generated_values.c"


make -C $FNV_LIB_DIRECTORY libfnv.a

gcc -Wall -o $OUTPUT_FILENAME -I$FNV_LIB_DIRECTORY -Ilookup_gen -L$FNV_LIB_DIRECTORY $FILES -lfnv

./$OUTPUT_FILENAME
