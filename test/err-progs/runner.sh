#!/bin/bash

export UBIK_INCLUDE=$PWD/../../lib:$PWD
../../bin/ubic $1 $1.out 1>/dev/null && exit 1
exit 0
