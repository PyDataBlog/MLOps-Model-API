#!/usr/bin/env bash

function error_info() {
    echo "--------------------------------------------------------------------"
    echo "| Error: invalid parameters number                                 |"
    echo "| Please excute this script like this:                             |"
    echo "| \$./burnning.sh mcu upload_port        | entry commandline mode   |"
    echo "| \$./burnning.sh r/w hex mcu upload_port| entry burnning/dump mode |"
    echo "--------------------------------------------------------------------"
}

if [ $# -eq 2 ]; then
    MCU=$1
    PORT=$2
    avrdude -C $AVRDUDE_CONF -P${PORT} -p${MCU} -c stk500v2 -t
elif [ $# -eq 4 ]; then
    MODE=$1
    HEX=$2
    MCU=$3
    PORT=$4
    test $MODE == "w"
    ret1=$?
    test $MODE == "r"
    ret2=$?
    if [ $ret1 -eq 0 ]; then
        echo "avrdude -D -C $AVRDUDE_CONF -p${MCU} -P${PORT} -c stk500v2 -b115200 -Uflash:w:${HEX}:i"
        avrdude -D -C $AVRDUDE_CONF -p${MCU} -P${PORT} -c stk500v2 -b115200 -Uflash:w:${HEX}:i
    elif [ $ret2 -eq 0 ]; then
        echo "avrdude -D -C $AVRDUDE_CONF -p${MCU} -P${PORT} -c stk500v2 -b115200 -Uflash:r:${HEX}:r"
        avrdude -D -C $AVRDUDE_CONF -p${MCU} -P${PORT} -c stk500v2 -b115200 -Uflash:r:${HEX}:r
        echo "Now, you can view the hex file with vim, set vim %!xxd mode to change to hex format"
    else
        error_info
    fi
else
    error_info
fi
