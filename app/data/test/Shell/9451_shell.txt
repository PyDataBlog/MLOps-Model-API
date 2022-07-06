#!/bin/bash
# Download and install Zend Debugger (test)

wget http://downloads.zend.com/studio_debugger/2011_04_10/ZendDebugger-20110410-linux-glibc23-x86_64.tar.gz
tar -xzf ZendDebugger-20110410-linux-glibc23-x86_64.tar.gz
cp ZendDebugger-20110410-linux-glibc23-x86_64/5_3_x_comp/ZendDebugger.so /usr/lib/php5/20090626/ZendDebugger.so

# Cleanup
rm ZendDebugger-20110410-linux-glibc23-x86_64.tar.gz
rm -rf ZendDebugger-20110410-linux-glibc23-x86_64

