#!/bin/bash

cd Torrent
docker-compose stop
cd ..
git pull
cd Torrent
docker-compose up -d
