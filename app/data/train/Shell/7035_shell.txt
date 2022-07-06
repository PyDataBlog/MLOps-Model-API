#!/usr/bin/env bash
set -e
ulimit -s 300999
echo "[" > report.txt
cat tests.txt | (cd critters; while read lng cmd; do
lng="$lng" cmd="$cmd" bash -c "${cmd}" >> ../log/compiler.log
done)
echo "]" >> report.txt
cat report.txt
