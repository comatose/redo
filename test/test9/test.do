#!/usr/bin/env sh
OUT_PATH=$(dirname $2)
redo-ifchange ${OUT_PATH}/test-sub1 ${OUT_PATH}/test-sub2
if [ -f flags ]; then
    redo-ifchange flags
else
    redo-ifcreate flags
fi
echo $0 $1 $2 > $3
