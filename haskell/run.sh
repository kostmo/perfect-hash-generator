#!/bin/bash -xe

stack build

rm -f hash-perfectly-strings-demo.tix
stack run hash-perfectly-strings-demo

rm -f hash-perfectly-ints-demo.tix
stack run hash-perfectly-ints-demo -- --count 500000

