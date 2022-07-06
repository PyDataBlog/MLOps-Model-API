#!/bin/bash

COMMAND="diff --ignore-space-at-eol"
NOFILE_MSG="Please supply a file to diff"

if [ -z $1 ]
then
    echo $NOFILE_MSG
else
    if [ "$1" = "-f" ]
    then
        if [ ! -e $VIM_OUTPUT_FILE ]
        then
            touch $VIM_OUTPUT_FILE
        fi
        if [ -z $2 ]
        then
            echo $NOFILE_MSG
        else
            $SOURCE_CONTROL $COMMAND $2 > $VIM_OUTPUT_FILE
        fi
    else
        $SOURCE_CONTROL $COMMAND $1
    fi
fi
