#!/usr/bin/env bash
for pkg in "async" "babel-cli" "babel-preset-es2015" "babel-preset-stage-2" "browser-sync" "cli-color" "cssnano" "fs-extra" "node-sass" "postcss-cli" "postcss-uncss" "postcss" "imagemin"
do
    npm i $pkg --save-dev
done
