#!/bin/bash

# Let's see if there's a docker-machine present
DOCKER_MACHINE=$(which docker-machine)
COUCHDB_HOST=localhost

if [ $? -eq 1 ]; then
    echo "No docker-machine; assume we're not on OSX";
else
    echo "Found docker-machine; assume we're on OSX"
    COUCHDB_HOST=$(DEBUG=false docker-machine ip default)
fi

#docker pull honeybadger/honeybadger:latest
docker run -d -p 8090:8090 -e COUCHDB_HOST=$COUCHDB_HOST --name honeybadger honeybadger #honeybadger/honeybadger
