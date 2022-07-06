#/bin/bash

uptime | awk '{print $10}' | perl -pe 's/,//'
