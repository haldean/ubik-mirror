#!/bin/sh

EXPELC=${EXPELC:-../../expelc/expelc}
RUNEXPEL=${RUNEXPEL:-../../runexpel/runexpel}
XL_FILE=$1

set -ex

export EXPEL_INCLUDE="${INCLUDE:-$PWD}"

$EXPELC $XL_FILE $XL_FILE.out
$RUNEXPEL $XL_FILE.out
