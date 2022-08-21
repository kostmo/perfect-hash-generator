#!/bin/bash -xe


SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

KEY_TYPE="${1:-int}"

CODEGEN_PARENT_DIR=$SCRIPT_DIR/c-demo/gen

rm -r $CODEGEN_PARENT_DIR


GENERATED_DIR=$CODEGEN_PARENT_DIR/$KEY_TYPE-keys

cd haskell
./demo-c-codegen.sh $GENERATED_DIR $KEY_TYPE
cd -

cd c-demo
./build-and-run.sh $GENERATED_DIR $KEY_TYPE
