#!/usr/bin/env sh
OUT_PATH=$(dirname $2)
redo-ifchange ${OUT_PATH}/test.a
echo $0 $1 $2 > $3
