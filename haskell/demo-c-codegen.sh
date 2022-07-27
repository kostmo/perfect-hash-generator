#!/bin/bash -xe

stack run gen-c-code-from-file -- --csv-filepath ../c-demo/data/key-value-pairs.csv --output-dir ../c-demo/lookup_gen

