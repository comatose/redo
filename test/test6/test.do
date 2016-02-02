#!/usr/bin/env sh
OUT_PATH=$(dirname $2)
DEPS=
for i in $(seq 1 15); do
    DEPS="${DEPS} ${OUT_PATH}/test-sub${i}.a"
done
redo-ifchange ${DEPS}
echo $0 $1 $2 > $3
