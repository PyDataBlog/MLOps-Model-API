applied=`hg qapp`
if [ -n "$applied" ]
then
  echo MQ contains applied patches, aborting.
  exit 1
fi

##########

date=`cat ../bridge/date`
echo '../bridge/date:' $date

hg update -C -d "$date"
if [ ! $? -eq 0 ]
then
  exit 1
fi

##########

info=`cat ../bridge/info`

hg qnew hg-import-info
hg qref -m "$info"
hg qpop

##########

patches=`cat ../bridge/patches`
hg qimport $patches

hg qpu hg-import-info

# HG abhors empty commits, so...
#tempFile='hg-abhors-an-empty-commit'
#touch $tempFile
#hg add $tempFile
#hg commit -m "touch $tempFile" > /dev/null
#hg rm $tempFile
#hg commit --amend -m "$info"  > /dev/null
