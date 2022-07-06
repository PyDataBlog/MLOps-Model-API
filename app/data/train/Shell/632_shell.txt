#!/bin/bash

# Creating the destination folder
# For the generated pandoc pdf
if [ ! -d "../dst" ]; then
  mkdir ../dst
fi

pandoc ./report.md -o ../dst/DSI_lab2.pdf
