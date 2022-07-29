#!/bin/bash -xe

GENERATED_DIR="${1:-local-gen}"
GENERATED_DATA_DIR=$GENERATED_DIR/data
GENERATED_CODE_DIR=$GENERATED_DIR/code

KEY_TYPE="${2:-int}"

STACK_ARGS="run gen-c-code-from-file -- --key-type $KEY_TYPE --csv-filepath $GENERATED_DATA_DIR/key-value-pairs.csv --output-dir $GENERATED_CODE_DIR"

stack $STACK_ARGS --write-csv
stack $STACK_ARGS

