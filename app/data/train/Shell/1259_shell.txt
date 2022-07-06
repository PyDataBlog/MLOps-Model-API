#!/bin/bash

if [ $# -eq 0 ] ; then
    echo "usage: clean_slave.sh <hadoop_dir_disk1> [<hadoop_dir_disk2> ...]"
    exit 0
fi

YARN_DIRS=
DATANODE_DIRS=
while [ $# -gt 0 ] ; do
    YARN_DIRS="${YARN_DIRS} $1/mapred/local $1/yarn/local"
    DATANODE_DIRS="${DATANODE_DIRS} $1/dfs/dn $1/hdfs/data"
    shift
done

# Yarn directories
rm -rf ${YARN_DIRS}

# DataNode directories
rm -rf ${DATANODE_DIRS}
