set -e

export UBIK_HOOKFILE=$PWD/../hook/hooks.txt
export UBIK_INCLUDE=$PWD
../bin/ubic $1 >/dev/null 2>/dev/null
