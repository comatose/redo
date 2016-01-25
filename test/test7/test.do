#!/usr/bin/env sh
OUT_PATH=$(dirname $2)
./redo-ifchange ${OUT_PATH}/test-sub1.a
echo $0 $1 $2 > $3
