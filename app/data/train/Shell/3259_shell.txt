#!/usr/bin/env bash

loadtest -k -n 50000 -c 100 "http://localhost:8080/ServerJava/home.html"
