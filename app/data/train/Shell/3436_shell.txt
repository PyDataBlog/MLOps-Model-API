#!/bin/sh

docker create -it --env="DISPLAY" \
                  --volume="/tmp/.X11-unix:/tmp/.X11-unix:rw" \
                  topy_env

export containerId=$(docker ps -l -q)

xhost +local:`docker inspect --format=' {{ .Config.Hostname }}' $containerId`
docker start $containerId -i
