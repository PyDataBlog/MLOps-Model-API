#!/bin/bash

# Nmap method is far more recommended than ping method

nmap -sP 128.178.121.211 -oG - | grep up
if  [ "$?" = "0" ]
then
	echo "host is up !"
else
	echo "host is down !"
fi

