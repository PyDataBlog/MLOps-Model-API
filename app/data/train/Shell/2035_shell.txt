#!/bin/bash

echo "writing 1000 entries"
for i in `seq 1 1000`;
do
    nanos=$(date +%N)
    curl -XPOST 'http://localhost:8086/write?db=demo' --data-binary "random,host=${HOSTNAME} value=${RANDOM}"
    curl -XPOST 'http://localhost:8086/write?db=demo' --data-binary "time,host=${HOSTNAME} value=${nanos}"
done
echo "done"
