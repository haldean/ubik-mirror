#!/bin/bash

export UBIK_HOOKFILE=$PWD/../../hook/hooks.txt
export UBIK_INCLUDE=$PWD/../../lib:$PWD
../../bin/ubic $1 >/dev/null 2>/dev/null && exit 1
exit 0
