#!/bin/bash -xe

stack build

rm -f hash-perfectly-strings-demo.tix
stack exec hash-perfectly-strings-demo

rm -f hash-perfectly-ints-demo.tix
stack exec hash-perfectly-ints-demo

