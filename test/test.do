#!/usr/bin/sh
OUT_PATH=$(dirname $2)
echo "OUT_PATH=" ${OUT_PATH}
./redo-ifchange ${OUT_PATH}/test-sub1 ${OUT_PATH}/test-sub2
echo $1 $2 > $3
