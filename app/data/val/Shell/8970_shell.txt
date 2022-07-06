#! /bin/bash

TOPIC_IMAGE=$1
OUTPUT=$2

OVERLAY_IMAGE="overlay.png"
WINDOW_SIZE="258x169"       # {w}x{h}
WINDOW_OFFSET="+7+23"       # +{x}+{y}

if [ "$OUTPUT" = "" ]; then
    echo "Usage: $0 IMAGE OUTPUT"
    exit
fi

convert_cmd="
    convert
    $TOPIC_IMAGE
    -resize $WINDOW_SIZE^
    -gravity center
    -crop $WINDOW_SIZE+0+0
    png:-
"

composite_cmd="
    composite
    -compose Dst_Over
    -gravity northwest
    -
    -geometry $WINDOW_OFFSET $OVERLAY_IMAGE
    $OUTPUT
"


set -ex
set -o pipefail

$convert_cmd | $composite_cmd
