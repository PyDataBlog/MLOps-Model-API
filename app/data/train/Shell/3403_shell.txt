#!/bin/sh

mkdir -p build-aux
aclocal \
	&& automake --gnu --add-missing \
	&& autoconf
