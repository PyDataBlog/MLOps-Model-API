#!/bin/bash

# Extract only the UKIDSS Zooniverse data from larger data dump

# K. Willett, UMN, 06 Nov 2013

OLDFILE=$1
OF1=${OLDFILE%.*}"_temp"
NEWFILE=${OLDFILE%.*}"_ukidss.csv"

# Delete lines with no responses for UKIDSS question 0

cut -d, -f1-5,36-47 $OLDFILE > $OF1

sed '/[en|zh|es]","a-/!d' $OF1 > $NEWFILE

read lines_old words_old chars_old filename_old <<< $(wc $OF1)
read lines_new words_new chars_new filename_new <<< $(wc $NEWFILE)
echo ""
echo "Extracted "$lines_new" UKIDSS classifications from "$lines_old" total in GZ."
echo ""

rm $OF1

# Check the header fields to make sure SLOAN, UKIDSS, FERENGI are unpopulated
# cat $NEWFILE | awk -F, '{print "CANDELS-0: "$6", SLOAN-0: "$24", UKIDSS-0: "$36", FERENGI-0: "$48}'
# echo ""

exit $?
