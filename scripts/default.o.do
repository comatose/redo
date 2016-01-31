if [ -e $(basename $2).cpp ]; then
   SRC=$(basename $2).cpp
elif [ -e $(basename $2).c ]; then
   SRC=$(basename $2).c
else
   exit 1
fi
redo-ifchange $SRC
mkdir -p $(dirname $2)
g++ -MD -MF $2.d -c -o $3 $SRC
read DEPS <$2.d
redo-ifchange ${DEPS#*:}
