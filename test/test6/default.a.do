#!/usr/bin/env sh
# for i in $(seq 1 1000000); do
#     i=$i
# done
redo-ifchange test.b
echo $0 $1 $2 > $3
