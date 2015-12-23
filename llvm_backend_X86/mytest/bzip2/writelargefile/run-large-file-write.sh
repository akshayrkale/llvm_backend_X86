#!/bin/bash

. params.sh

exe="./sequential_write"

if [ ! -e $exe ] ; then
    echo "Stop! You must run 'make' first!"
    exit 1
fi


echo "beginning sequential write test..."
$exe -o$mnt/$input -b$io_size -n$random_buffers -s$f_size
echo "done!"
