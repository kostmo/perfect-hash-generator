#!/bin/bash -xe

mkdir -p output
stack run measure-hash-spread -- -o output/$1.svg --selection $1

