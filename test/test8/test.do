#!/usr/bin/env sh
OUT_PATH=$(dirname $2)
DEPS=
for i in $(seq 1 5); do
    DEPS="${DEPS} ${OUT_PATH}/test-sub${i}.a"
done
redo-ifchange ${DEPS}
if [ -f flags ]; then
    redo-ifchange flags
else
    redo-ifcreate flags
fi
echo $0 $1 $2 > $3
