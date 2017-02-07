#!/bin/sh

iters=50

if [[ -e bench-results ]]
then
        rm -r bench-results
fi
mkdir -p bench-results

for f in *.uk
do
        ../../bin/ubic $f bench-results/${f%.uk}.ub >/dev/null 2>/dev/null
done

function runall()
{
        for (( i = 0 ; i < $iters ; i = i + 1 ))
        do
                echo $(expr $iters - $i)
                for f in bench-results/*
                do
                        ../../bin/ubik $f >/dev/null 2>/dev/null
                done
        done
}

time runall
