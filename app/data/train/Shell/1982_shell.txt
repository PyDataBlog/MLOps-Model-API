#!/bin/sh

function print_usage
{
    echo "usage: compose.sh [[[--dev] [-d] [-v]] | [-h]] [CMD]"
}


DEV_MODE=false
COMPOSE_FILE=docker-compose.prod.yml
PROJECT_NAME=nonaginta-novem
DEAMON_MODE=false
VERBOSE_MODE=false
ACTION=$BASH_ARGV

while [ "$1" != "" ]; do
    case $1 in
        --dev ) DEV_MODE=true
            ;;
        -d | --daemon ) DAEMON_MODE=true
            ;;
        -v | --verbose) VERBOSE_MODE=true
            ;;
        -h | --help ) print_usage
            exit
            ;;
        --* | -* ) print_usage
            exit 1
    esac
    shift
done

if [ "$DEV_MODE" == "true" ]; then
    COMPOSE_FILE=docker-compose.dev.yml
fi

CMD=docker-compose
CMD+=" -p $PROJECT_NAME"
CMD+=" -f $COMPOSE_FILE"
CMD+=" $ACTION"
if [ "$DAEMON_MODE" == "true" ]; then
    CMD+=" -d"
fi

if [ "$VERBOSE_MODE" == "true" ]; then
    $CMD
else
    $($CMD)
fi
