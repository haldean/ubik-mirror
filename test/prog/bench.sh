#!/bin/sh

iters=50
export UBIK_HOOKFILE=../../hook/hooks.txt
export UBIK_INCLUDE=../../lib:.

if [[ -e bench-results ]]
then
        rm -r bench-results
fi
mkdir -p bench-results

for f in $(find . -name '*.uk')
do
    f=$(basename $f)
    out="bench-results/${f%.uk}.ub"
    ../../bin/ubic $f $out >/dev/null 2>/dev/null || rm -f $out
done

function runall()
{
    for (( i = 0 ; i < $iters ; i = i + 1 ))
    do
        echo $(expr $iters - $i)
        find bench-results -name '*.ub' -print0 | \
            xargs -0 -n1 ../../bin/ubik >/dev/null 2>/dev/null
    done
}
time runall
