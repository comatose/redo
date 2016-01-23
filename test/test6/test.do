#!/usr/bin/env sh
OUT_PATH=$(dirname $2)
./redo-ifchange ${OUT_PATH}/test-sub1.a ${OUT_PATH}/test-sub2.a ${OUT_PATH}/test-sub3.a ${OUT_PATH}/test-sub4.a ${OUT_PATH}/test-sub5.a
echo $0 $1 $2 > $3
