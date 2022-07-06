#!/bin/bash
if [ "$1" = "" ]
then
  echo "Usage: $0 <number of iterations> [filesizeMB]"
  exit
fi

FILE_SIZE_MB=1048576

if [ $2 ]
then
 FILE_SIZE_MB=$2
fi

SYNCAPPNAME="syncRead.js"
ASYNCAPPNAME="asyncRead.js"

echo "Creating file 1..."
dd if=/dev/zero of=archivo1.txt bs=1048576 count=$FILE_SIZE_MB
echo "Creating file 2..."
dd if=/dev/zero of=archivo2.txt bs=1048576 count=$FILE_SIZE_MB
echo "Files created"

echo "Executing node programs... " 

echo "Executing $SYNCAPPNAME : $1 iterations"
SYNCTIME=`(time seq $1 | xargs -Iz node $SYNCAPPNAME) 2>&1 | grep real`
echo $SYNCTIME

echo "Excecuting $ASYNCAPPNAME : $1 iterations"
ASYNCTIME=`(time seq $1 | xargs -Iz node $ASYNCAPPNAME) 2>&1 | grep real`
echo $ASYNCTIME

echo "Delete files..."
rm -f archivo*
echo "Files deleted"