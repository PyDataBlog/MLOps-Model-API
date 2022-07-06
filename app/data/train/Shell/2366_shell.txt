#!/bin/bash
#
# update translation files
#

if [ ! -d "anki" ]
then
    echo "Please run this from the anki-i18n project root directory"
    exit
fi

all=all.files
echo "Updating anki.pot..."
for i in ../dtop/anki/{*.py,importing/*.py,template/*.py}; do
    echo $i >> $all
done
for i in ../dtop/aqt/{*.py,forms/*.py}; do
    echo $i >> $all
done

xgettext -s --no-wrap --files-from=$all --output=anki/anki.pot
rm $all
