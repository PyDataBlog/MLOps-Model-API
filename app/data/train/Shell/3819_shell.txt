#!/bin/bash

# Copyright (c) 2016, Codrin-Victor Poienaru <cvpoienaru@gmail.com>.
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
# 
# 1. Redistributions of source code must retain the above copyright notice, this
# list of conditions and the following disclaimer.
# 
# 2. Redistributions in binary form must reproduce the above copyright notice,
# this list of conditions and the following disclaimer in the documentation
# and/or other materials provided with the distribution.
# 
# This software is provided by the copyright holders and contributors "as is"
# and any express or implied warranties, including, but not limited to, the
# implied warranties of merchantability and fitness for a particular purpose are
# disclaimed. In no event shall the copyright holder or contributors be liable
# for any direct, indirect, incidental, special, exemplary, or consequential
# damages (including, but not limited to, procurement of substitute goods or
# services; loss of use, data, or profits; or business interruption) however
# caused and on any theory of liability, whether in contract, strict liability,
# or tort (including negligence or otherwise) arising in any way out of the use
# of this software, even if advised of the possibility of such damage.

# Directory structure
export EDP_ROOT=~/Work/Projects/EssentialDevPack
export EDP_BIN=$EDP_ROOT/bin
export EDP_ETC=$EDP_ROOT/etc
export EDP_INCLUDE=$EDP_ROOT/include
export EDP_LIB=$EDP_ROOT/lib
export EDP_SCRIPT=$EDP_ROOT/script
export EDP_SRC=$EDP_ROOT/src

# Environment paths
export C_INCLUDE_PATH=$EDP_INCLUDE
export LIBRARY_PATH=$EDP_LIB
export LD_LIBRARY_PATH=$EDP_LIB
export PATH=$PATH:$EDP_BIN

# File system permission mask
export EDP_MASK=0022

# Build options
export EDP_DEBUG=true
export EDP_BUILD_THREADS=16

unset EDP_FLAGS
if $EDP_DEBUG; then
	export EDP_FLAGS=-g
fi
