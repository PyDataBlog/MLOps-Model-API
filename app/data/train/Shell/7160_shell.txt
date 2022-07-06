#!/bin/bash
cat $0 | awk '{ for (i=NF; i>0; i--) printf("%s ",$i); }'
