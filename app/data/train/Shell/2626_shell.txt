#!/bin/bash

DOCKER="nvidia-docker"
DOCKER_RUN="-it"
IMAGE="aleozlx/tkstack:latest"

# Expose ports
let "P=8888"

PORTS="-p $((P++)):8888"

# Define file locations to be mounted from host here
PROJECT="$(readlink -f .)"
SCRIPTS="$(readlink -f ./notebooks)"
DATASETS="$(readlink -f ../datafiles)"

VOLUMNS="-v $PROJECT:/workspace/project -v $DATASETS:/workspace/datasets -v $SCRIPTS:/workspace/scripts"

# Launch
set -x
$DOCKER run $DOCKER_RUN $PORTS $VOLUMNS $IMAGE $DOCKER_CMD
