#!/bin/sh

UBIC=${UBIKC:-../../ub/ubic}
UBIK=${RUNUBIK:-../../ubik/ubik}
XL_FILE=$1

set -ex

export UBIK_INCLUDE="${INCLUDE:-$PWD}"

$UBIC $XL_FILE $XL_FILE.out
$UBIK $XL_FILE.out
