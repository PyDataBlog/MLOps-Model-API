#!/bin/sh
foo='LABEL co.tutum.yml="'
foo1=$(yaml2json /image/tutum.yml | tr "\n" " " | sed 's/"/\\"/g')
foo2='"'
echo "" >> /image/Dockerfile
echo $foo$foo1$foo2 >> /image/Dockerfile