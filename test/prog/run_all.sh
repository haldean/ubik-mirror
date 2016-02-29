#!/bin/sh

set -ex

for xl_file in test/prog/*.xl; do
        if grep skip-run $xl_file; then
                continue
        fi
        dist/expelc $xl_file a.out
        dist/runexpel a.out
        rm a.out
done
