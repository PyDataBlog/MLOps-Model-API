#!/bin/bash
#Author: Renan Silva (renanvice@gmail.com)
#License: Apache Version 2.0

### Colors
GREEN="\033[1;32m"
YELLOW="\033[1;33m"
BLUE="\033[1;34m"
RED="\033[1;31m"
END="\033[m"
###
WITHOUTPROBLEM="$GREN[+]$END"
INFORMATION="$BLUE[+]$END"
WARNING="$YELLOW[-]$END"
PROBLEM="$RED[-]$END"




# Check if java is already installed

if [ -d "/usr/java/" ]
then
  echo -e "
$INFORMATION Directory already exists \n
$INFORMATION Removing old version"
  rm -rf /usr/java/*
else
  echo -e "
$WARNING No such file or directory \n
$WITHOUTPROBLEM Creating directory"
  mkdir /usr/java/
fi

cd /tmp/

# check OS architecture

ARCH=$(getconf LONG_BIT)
if [ "$ARCH" = "32" ]
then
  echo -e "
$INFORMATION Found 32bits \n
$INFORMATION Downloading Java 32bits"
  wget "http://javadl.sun.com/webapps/download/AutoDL?BundleId=75250"  -O java.tar.gz
else
  echo -e "$INFORMATION Found 64bits"
  echo -e "$INFORMATION Downloading Java 64bits"
  wget "http://javadl.sun.com/webapps/download/AutoDL?BundleId=75252"  -O java.tar.gz
fi

# unpacking tar.gz
cd /usr/java/
tar zxvf /tmp/java.tar.gz

  echo -e "
$WITHOUTPROBLEM Congradulations Java successfully installed"

# settings browser plugin

echo -e "
Setting Java Web Plugin \n
Choose your Browser: \n
0 - Mozilla Firefox/Iceweasel \n
1 - Chromium Browser \n
2 - Google Chrome \n
q - Sair"

read ANSWER

case $ANSWER in

0|1)
  # check if plugin already exist
  if [ -f "/usr/lib/mozilla/plugins/libnpjp2.so" ]
  then
    echo -e "
  $WARNING Plugin already exists \n
  $INFORMATION Removing old vestion"
    rm -rf /usr/lib/mozilla/plugins/libnpjp2.so
  fi
  if [ "$ARCH" = "32" ]
  then
    ln -s /usr/java/jre*/lib/i386/libnpjp2.so /usr/lib/mozilla/plugins/
    echo -e "$WITHOUTPROBLEM Plugin successfully installed"
  else 
    ln -s /usr/java/jre*/lib/amd64/libnpjp2.so /usr/lib/mozilla/plugins
    echo -e "$WITHOUTPROBLEM Plugin successfully installed"
  fi
;;
2)
  if [ -f "/opt/google/chrome/plugins/libnpjp2.so" ]
  then
    echo -e "
  $WARNING Plugin already exists \n
  $INFORMATION Removing old vestion"
    rm -r /opt/google/chrome/plugins/libnpjp2.so
  else
    echo -e "
  $INFORMATION Directory already exists \n
  $INFORMATION Removing old version"
    mkdir /opt/google/chrome/plugins/
  fi
  if [ "$ARCH" = "32" ]
  then
    ln -s /usr/java/jre*/lib/i386/libnpjp2.so /opt/google/chrome/plugins/
    echo -e "$WITHOUTPROBLEM Plugin successfully installed"
  else 
    ln -s /usr/java/jre*/lib/amd64/libnpjp2.so /opt/google/chrome/plugins/
    echo -e "$WITHOUTPROBLEM Plugin successfully installed"
  fi
;;
q)
  echo -e "$INFORMATION Closing"
;;
*)
  echo -e "$PROBLEM unrecognized option. Try again"
  continue
;;
esac
