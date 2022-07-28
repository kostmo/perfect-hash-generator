#!/bin/bash -xe

GENERATED_DIR="${1:-local-gen}"
GENERATED_DATA_DIR=$GENERATED_DIR/data
GENERATED_CODE_DIR=$GENERATED_DIR/code


stack run gen-c-code-from-file -- --write-csv --csv-filepath $GENERATED_DATA_DIR/key-value-pairs.csv --output-dir $GENERATED_CODE_DIR

stack run gen-c-code-from-file -- --csv-filepath $GENERATED_DATA_DIR/key-value-pairs.csv --output-dir $GENERATED_CODE_DIR

