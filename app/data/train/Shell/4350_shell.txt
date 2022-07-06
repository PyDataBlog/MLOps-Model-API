#!/bin/bash

docker build --pull --tag="kurron/monitor-postgresql:latest" .
docker images
