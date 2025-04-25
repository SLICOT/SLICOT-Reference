#!/bin/sh

ldir=$(pwd)
for i in *
do
    if [ ! -d "$i" ]; then
        continue
    fi
    cd "$i"
    sh ./build.sh
    cd "$ldir"
done


