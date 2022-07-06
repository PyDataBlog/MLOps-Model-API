#! /bin/bash
cd /opt/deploy/pyconjp-android/Docker/
# Clean
make clean
# Build
make build
# Update to git
make git
