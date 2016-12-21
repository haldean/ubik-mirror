#!/bin/bash

export UBIK_HOOKFILE=$PWD/../../hook/hooks.txt
./$1 >/dev/null 2>/dev/null
exit $?
