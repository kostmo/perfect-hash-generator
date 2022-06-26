#!/bin/bash -xe

stack run mk-diagram -- --output perfect-hash-generator/docs/images/algorithm-diagram.svg

stack haddock --force-dirty perfect-hash-generator 
