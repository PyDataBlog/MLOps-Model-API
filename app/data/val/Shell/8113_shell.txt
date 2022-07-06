#!/bin/bash

# Nmap method is far more recommended than ping method

#nmap -sP 128.178.121.80-86
nmap -sP 128.178.121.0/24 # will scan all the network
