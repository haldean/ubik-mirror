#!/bin/sh

export EXPEL_INCLUDE=test/prog
failed=0

for xl_file in test/prog/*.xl; do
    if grep skip-run $xl_file >/dev/null; then
        echo "skip: $xl_file"
        continue
    fi
    dist/expelc $xl_file a.out
    if [ "$?" != "0" ]; then
        failed=1
        echo "fail: $xl_file in compilation"
        continue
    fi
    dist/runexpel a.out >/dev/null
    if [ "$?" != "0" ]; then
        failed=1
        echo "fail: $xl_file"
    else
        rm a.out
        echo "ok:   $xl_file"
    fi
done
