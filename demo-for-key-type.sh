#!/bin/bash -xe


SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

KEY_TYPE="${1:-int}"


GENERATED_DIR=$SCRIPT_DIR/c-demo/gen/$KEY_TYPE-keys

cd haskell
./demo-c-codegen.sh $GENERATED_DIR $KEY_TYPE
cd -

cd c-demo
./build-and-run.sh $GENERATED_DIR $KEY_TYPE
