#!/usr/bin/env sh
./redo-ifchange $2.b $2.c
echo $0 $1 $2 > $3
sleep 1