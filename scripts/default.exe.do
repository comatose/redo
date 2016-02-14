#!/usr/bin/env sh

SRC=$2
OUT=$3

call_redo()
{
    if [ -e $1 ]; then
        redo-ifchange $1
        mkdir -p $(dirname $SRC)
        $2 $3 -o $OUT $1
        read DEPS < $SRC.d
        redo-ifchange ${DEPS#*:}
        exit 0
    else
        redo-ifcreate $1
    fi
}

# find an object file in the directory where the target is generated.
call_redo ${2%.exe}.o ${LD:=ld} "$LDFLAGS"

# find sources in the current directory.
call_redo $(basename $2).o ${LD:=ld} "$LDFLAGS"
call_redo $(basename $2).cpp ${CXX:=g++} "-MMD -MF $SRC.d $CXXFLAGS $LDFLAGS"
call_redo $(basename $2).cc ${CXX:=g++} "-MMD -MF $SRC.d $CXXFLAGS $LDFLAGS"
call_redo $(basename $2).c ${CC:=gcc} "-MMD -MF $SRC.d $CFLAGS $LDFLAGS"
