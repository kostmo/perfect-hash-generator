#!/bin/bash -xe


OUTPUT_FILENAME=demo


make -C fnv

gcc -Wall  -o $OUTPUT_FILENAME -Ifnv -Lfnv demo.c -lfnv

./$OUTPUT_FILENAME
