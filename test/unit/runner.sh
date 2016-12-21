#!/bin/bash

export UBIK_HOOK_ROOT=$PWD/../../hook
./$1 >/dev/null 2>/dev/null
exit $?
