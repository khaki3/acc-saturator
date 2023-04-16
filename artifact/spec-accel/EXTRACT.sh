#!/bin/bash

: ${COMP=nvhpc}
: ${TAG=-sat}

LABEL='_omp_|_gpu|nvkernel_'

for log in `find -mindepth 2 | grep $COMP$TAG.log | sort -n | grep $COMP`;
do
    nline=`cat $log | egrep $LABEL | wc -l`
    nline=$((nline / 3))
    # echo $log
    # echo $nline
    for i in `seq 0 2`; do
        cat $log | egrep $LABEL | tail +$((i * nline + 1)) | head -$nline |
            awk -v OFMT='%.0f' '{ sum += $2 } END { print sum }'
    done | sort -n | head -1
done | tr '\n' ',' | sed -e 's/,$/\n/'
