#!/usr/bin/sh
OUT_PATH=$(dirname $2)
./redo-ifchange ${OUT_PATH}/test-sub1 ${OUT_PATH}/test-sub2
if [ ! -f flags ]; then
    ./redo-ifcreate flags
else
    ./redo-ifchange flags
fi
echo $1 $2 > $3
