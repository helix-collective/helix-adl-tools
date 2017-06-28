#!/bin/sh
set -e
ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"/..
stack build adl-compiler
stack exec adlc -- haskell -O $ROOT/src --package=ADL $ROOT/adl/sql/Schema.adl
