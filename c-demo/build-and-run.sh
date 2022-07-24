#!/bin/bash -xe

cd fnv
make
cd -

gcc -Wall  -o hello -Ifnv -Lfnv hello.c -lfnv

./hello
