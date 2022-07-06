# Copyright (c) 2015-2016 Contributors as noted in the AUTHORS file
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

#!/bin/sh

set -e

echo "http://dl-4.alpinelinux.org/alpine/edge/testing" >> /etc/apk/repositories

apk --update add python py-pip py-gevent py-zmq
pip install --no-cache-dir pebble ipaddress
pip install --no-cache-dir https://github.com/zeromq/pyre/archive/master.zip
pip install --no-cache-dir https://github.com/aglyzov/netcall/archive/master.zip
pip install --no-cache-dir pytest pytest-cov

rm /var/cache/apk/*
