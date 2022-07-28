#!/bin/bash -xe


SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

GENERATED_DIR=$SCRIPT_DIR/c-demo/gen

cd haskell
./demo-c-codegen.sh $GENERATED_DIR
cd -

cd c-demo
./build-and-run.sh $GENERATED_DIR
