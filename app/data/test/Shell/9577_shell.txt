#!/bin/bash

# Install jq 1.4, if it's not there already
JQ_VERSION=$(jq --version)
if [[ "$JQ_VERSION" =~ "1.4" ]]; then
    echo "jq version 1.4 detected"
else
    wget --output-document="${HOME}/bin/jq" https://github.com/stedolan/jq/releases/download/jq-1.4/jq-linux-x86_64
    chmod u+x ${HOME}/bin/jq
fi
