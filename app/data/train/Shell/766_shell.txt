#!/bin/bash
set -e
set -o pipefail

stylus styles/app.styl --out static/css --compress -u nib --import nib
