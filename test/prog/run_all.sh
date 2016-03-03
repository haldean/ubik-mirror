#!/bin/sh

set -e

for xl_file in test/prog/*.xl; do
    if grep skip-run $xl_file >/dev/null; then
        echo "skip: $xl_file"
        continue
    fi
    dist/expelc $xl_file a.out
    dist/runexpel a.out >/dev/null
    rm a.out
    echo "ok:   $xl_file"
done
