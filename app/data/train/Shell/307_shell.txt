#!/bin/bash

SIGNAL=~/local-area/Stop4Body/nTuples_v2016-07-12/T2DegStop_300_270_bdt.root
BACKGROUND=~/local-area/Stop4Body/nTuples_v2016-07-12/Background_bdt.root

if [[ -f ${SIGNAL} && -f ${BACKGROUND} ]] ; then
  trainMVA --signalFile ${SIGNAL} --backgroundFile ${BACKGROUND}
  root runTMVAGui.C
fi

