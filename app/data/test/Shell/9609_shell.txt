#!/bin/bash -xe

case "$1" in
  master)
if ! test -e /usr/src/app/.dbcreate ; then
  buildbot create-master /usr/src/app/master
  touch /usr/src/app/.dbcreate
fi
exec buildbot start --nodaemon /usr/src/app/master
  ;;
  slave)
if ! test -e /usr/src/app/.dbcreate ; then
  buildslave create-slave buildarea buildbot.python.org:9020 slavename slavepasswd
  ;;
