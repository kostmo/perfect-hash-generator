#!/bin/bash -xe


cd haskell
./demo-c-codegen.sh
cd -

cd c-demo
./build-and-run.sh
