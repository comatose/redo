#!/usr/bin/env sh
OUT_PATH=$(dirname $2)
./redo-ifchange ${OUT_PATH}/test-sub1 ${OUT_PATH}/test-sub2.a ${OUT_PATH}/test-sub3.a ${OUT_PATH}/test-sub4.a ${OUT_PATH}/test-sub5.a
if [ -f flags ]; then
    ./redo-ifchange flags
else
    ./redo-ifcreate flags
fi
echo $0 $1 $2 > $3
