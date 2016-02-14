#!/usr/bin/env sh

SRC=$2
OUT=$3

call_redo()
{
   if [ -e $1 ]; then
      redo-ifchange $1
      $2 $3 -c -MMD -MF $SRC.d -o $OUT $1
      read DEPS < $SRC.d
      redo-ifchange ${DEPS#*:}
      exit 0
   fi
}

# find sources in the current directory.
call_redo $(basename $2).cpp ${CXX:=g++} "$CXXFLAGS"
call_redo $(basename $2).cc ${CXX:=g++} "$CXXFLAGS"
call_redo $(basename $2).c ${CC:=gcc} "$CFLAGS"
