#!/bin/bash
#
# start_proxy.sh
# Copyright (C) 2018 jack <jack@vm10.davidandjack.cn>
#
# Distributed under terms of the MIT license.
#

socat "TCP-LISTEN:443,fork,reuseaddr,su=nobody" "TCP6:chexie.net:443" &
