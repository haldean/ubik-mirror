#!/bin/sh

UBIC=${UBIKC:-../../ub/ubic}
UBIK=${RUNUBIK:-../../ubik/ubik}
UBIK_FILE=$1

set -ex

export UBIK_INCLUDE="${INCLUDE:-$PWD}"

$UBIC $UBIK_FILE $UBIK_FILE.out
$UBIK $UBIK_FILE.out
