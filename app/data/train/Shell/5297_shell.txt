#! /bin/bash
SOURCEDIR=./_posts0
TARGETDIR=./_posts
rm -rf $TARGETDIR
mkdir $TARGETDIR
NEWDATE=`date -d "yesterday 13:00" +%Y-%m-%d`
for FILE in `ls $SOURCEDIR/*.markdown`; do
	FNAME=`echo $FILE|sed -e "s|.*/||g"`
	#echo $FNAME
	cat $FILE|sed -e "s|MYPOSTDATE|"$NEWDATE"|g" > $TARGETDIR/$FNAME 
done
