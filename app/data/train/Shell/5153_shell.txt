#!/usr/bin/env bash

loadtest -k -n 50000 -c 100 "http://127.0.0.1:3000/"
