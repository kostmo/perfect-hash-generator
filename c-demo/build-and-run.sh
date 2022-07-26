#!/bin/bash -xe


OUTPUT_FILENAME=demo
FNV_LIB_DIRECTORY=fnv


make -C $FNV_LIB_DIRECTORY libfnv.a

gcc -Wall -o $OUTPUT_FILENAME -I$FNV_LIB_DIRECTORY -L$FNV_LIB_DIRECTORY demo.c utils.c lookup.c -lfnv

./$OUTPUT_FILENAME
