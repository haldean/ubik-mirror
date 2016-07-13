#!/bin/bash
set -e

export UBIK_INCLUDE=$PWD/../../lib:$PWD
../../bin/ubic $1 $1.out 2>/dev/null
../../bin/ubik $1.out >/dev/null 2>/dev/null
