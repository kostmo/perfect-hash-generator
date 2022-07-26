#!/bin/bash -xe

stack run gen-c-code-from-file -- --input-filepath ../c-demo/key-value-pairs.csv --output-filepath ../c-demo/lookup_table.c

