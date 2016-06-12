#!/bin/bash
set -e

export UBIK_INCLUDE=$PWD
../../bin/ubic $1 $1.out
../../bin/ubik $1.out >/dev/null 2>/dev/null
