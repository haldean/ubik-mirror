#!/bin/sh

UBIC=${UBIKC:-../../bin/ubic}
UBIK=${RUNUBIK:-../../bin/ubik}
UBIK_FILE=$1

set -ex

export UBIK_INCLUDE="${INCLUDE:-$PWD}"

$UBIC $UBIK_FILE $UBIK_FILE.out
$UBIK $UBIK_FILE.out
