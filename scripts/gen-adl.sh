#!/bin/bash
set -e
ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"/..

if [ "${NIX_PATH-}" ]; then
    STACK="stack --nix"
else
    STACK="stack"
fi

$STACK build adl-compiler
$STACK exec adlc -- haskell -O $ROOT/lib --package=ADL $ROOT/adl/sql/schema.adl
