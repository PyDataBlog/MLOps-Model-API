#!/bin/sh

/etc/init.d/hobbit start & 2>1;
supervisord -n
