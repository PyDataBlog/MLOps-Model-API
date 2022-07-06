#!/bin/bash

comm -23 \
        <(find /etc /usr \( -type d -printf '%p/\n' -o -print \) | sort ) \
        <(pacman -Qlq | sort -u )
