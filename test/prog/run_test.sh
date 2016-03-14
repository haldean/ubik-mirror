#!/bin/sh

EXPELC=${EXPELC:-../../expelc/expelc}
RUNEXPEL=${RUNEXPEL:-../../runexpel/runexpel}
XL_FILE=$1

set -ex

export EXPEL_INCLUDE="$PWD"

$EXPELC $XL_FILE a.out
$RUNEXPEL a.out
