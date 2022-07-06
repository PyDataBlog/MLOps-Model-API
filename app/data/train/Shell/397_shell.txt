#!/usr/bin/env bash

source set-glibc-version.sh

docker cp glibc-builder:/glibc-bin-$GLIBC_VERSION.tar.gz ./
