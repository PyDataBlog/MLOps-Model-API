#!/bin/bash

directory="/media/HDD/Data/Backups/Daily"
month=$(date '+%Y-%m')

for folder in $(ls "$directory/")
do
    if [ -d "$directory/$folder/$month" ]
    then
        echo $folder
        rdiff-backup --list-increments "$directory/$folder/$month" | grep -v '^Found' | grep -v '^Current'
        echo
        echo
    fi
done
