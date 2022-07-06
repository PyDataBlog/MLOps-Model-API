#!/bin/sh -e

prj_make=$( pwd )/.git/prj_make
bin_make=$( which make.sh )

schroot.sh $prj_make $bin_make "$@"
