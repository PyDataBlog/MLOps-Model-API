#! /bin/bash

BASHRC="$HOME/.bashrc"

echo "# java setting" >> $BASHRC
echo "export JAVA_HOME=\$(/usr/libexec/java_home)" >> $BASHRC
echo "" >> $BASHRC
