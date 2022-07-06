#!/bin/bash

export DISPLAY=":99"

linux_wd=$1
workspace_dir=$2

qmake_58="/home/ubuntu/Qt/5.8/gcc_64/bin/qmake"

mkdir -p /home/ubuntu/workspace/$linux_wd

# running test
cd /home/ubuntu/workspace/$linux_wd/
$qmake_58 $workspace_dir
make
make check
