#!/usr/bin/env sh
for i in $(seq 1 1000000); do
    i=$i
done
printenv | grep "REDO" >> $3
echo $0 $1 $2 >> $3
