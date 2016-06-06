#!/bin/bash
set -e

../../bin/ubic $1 $1.out
../../bin/ubik $1.out >/dev/null 2>/dev/null
