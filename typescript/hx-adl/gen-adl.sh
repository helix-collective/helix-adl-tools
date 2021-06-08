#!/bin/bash
set -eu


SCRIPT_DIR="$(cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HXADL=$SCRIPT_DIR/adl
ONE_DIR=$HOME/Helix/onederful/onederful/mobile
ADL_RT_PACKAGE=au.com.helixta.adl.runtime
SEARCHDIR=$HOME/Develop/testadl
# Standard flags we use when generating java
ADL_JAVA_FLAGS="--rtpackage $ADL_RT_PACKAGE --suppress-warnings-annotation all"
ADLC=$HOME/.local/bin/adlc
ADL_FILES=`find $SEARCHDIR -name '*.adl'`
OUTDIR=$SEARCHDIR/adl

$ADLC java $ADL_JAVA_FLAGS \
  --searchdir $SEARCHDIR \
  --searchdir $HXADL \
  --package com.onederful.onederful_app.adl \
  --outputdir $OUTDIR/java \
  --include-rt \
  $ADL_FILES


ADLC=$HOME/.local/bin/adlc node --trace-warnings $SCRIPT_DIR/build/main.js react-native-android \
  --searchdir $SEARCHDIR \
  --searchdir $HXADL \
  --package com.onederful.onederful_app.adl \
  --javadir $OUTDIR/java \
  --tsdir $OUTDIR/typescript \
  $ADL_FILES