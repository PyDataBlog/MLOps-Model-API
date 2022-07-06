#!/bin/bash

set -e

echo "This script is meant to be run inside the vagrant machine"
echo -n "Please confirm this is the case [y/N] "
read confirm

if [ "$confirm" != "y" ] && [ "$confirm" != "Y" ]; then
  exit 1;
fi

sudo apt-get install -y python-pip git libyaml-dev libpython2.7-dev

repository_list="
  git@github.com:confpack/confpacker.git
  git@github.com:confpack/examples.git
  git@github.com:confpack/runtime.git
  git@github.com:confpack/cpcommon.git
  git@github.com:confpack/docs.git
  git@github.com:confpack/apt-server.git
"

for r in $repository_list; do
  git clone --recursive $r
done

for d in $(find . -maxdepth 1 -type d); do
  pushd $d
    # TODO: might move this to a virtualenv per project
    if [ -f requirements.txt ]; then
      pip install --user -r requirements.txt
    fi
  popd
done
