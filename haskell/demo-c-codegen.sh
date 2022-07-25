#!/bin/bash -xe

stack run gen-c-code-from-file -- --input-filepath ../c-demo/key-value-pairs.csv --output-filepath lookup_table.c

