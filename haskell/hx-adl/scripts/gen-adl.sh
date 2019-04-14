#!/bin/bash
set -e
HXADL="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"/..
cd $HXADL

stack build --docker adl-compiler
stack exec --docker adlc -- haskell -O $HXADL/lib --package=ADL $HXADL/adl/sql/schema.adl
