#!/bin/sh

mkdir -p heroku
wget https://cli-assets.heroku.com/heroku-cli/channels/stable/heroku-cli-linux-x64.tar.gz -O ./heroku/heroku.tar.gz
tar -xvzf heroku/heroku.tar.gz
heroku-cli*/bin/heroku --version
