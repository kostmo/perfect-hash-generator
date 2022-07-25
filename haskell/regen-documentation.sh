#!/bin/bash -xe

PACKAGE_NAME=perfect-hash-generator
PACKAGE_VERSION=$(yq .version perfect-hash-generator/package.yaml)
VERSIONED_PACKAGE_NAME=$PACKAGE_NAME-$PACKAGE_VERSION

stack run mk-diagram -- --output $PACKAGE_NAME/docs/images/algorithm-diagram.svg

stack haddock --force-dirty $PACKAGE_NAME 
ROOT_DOCS_DIR=$(stack path --local-doc-root)
PACKAGE_DOCS_INDEX=$ROOT_DOCS_DIR/$VERSIONED_PACKAGE_NAME/index.html

google-chrome $PACKAGE_DOCS_INDEX