#!/bin/sh

# Check and delete broken symlinks
find -L /usr/local/bin -type l -exec rm -i {} +

# Create symlinks
PWD=$(pwd)
ln -sf ${PWD}/scripts/* /usr/local/bin
