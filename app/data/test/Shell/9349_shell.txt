#!/bin/bash

DIR=$(dirname "$0")
DATADIR="../../data"
ABSDIR=$(realpath "$DATADIR")
OUTLIST="$DIR/list.txt"

EXT="*.wav"


find "$ABSDIR" -name "$EXT" > "$OUTLIST"

