#
# This is a forum post on : 
# http://ubuntuforums.org/showthread.php?t=2195496
# by SeijiSensei
#

#!/bin/sh

BACKUPDIR=/root/backups
ROTATE=8
USER=root
HOST=localhost

mkdir -p $BACKUPDIR

# read in a hostname from the command line if it exists
if [ "$1" != "" ]
then
    HOST=$1
fi

LOG="$BACKUPDIR/mysql-$HOST.`date +%y%m%d`.log"
STALELOG="$BACKUPDIR/mysql-$HOST.`date +%y%m%d --date="$ROTATE days ago"`.log"

echo -n `date -R` >> $LOG
echo " MySQL backup procedure for $HOST starting" >> $LOG

CURRENT="$BACKUPDIR/$HOST.`date +%y%m%d`.mysql"
echo "Creating current backup file $CURRENT" >> $LOG

# run the backup and compress it
/usr/bin/mysqldump -u $USER --all-databases > $CURRENT
gzip $CURRENT

# retain monthly copy on 15th; delete state logs
if [ "`date +%d`" != "15" ]
then
    STALE="$BACKUPDIR/$HOST.`date +%y%m%d --date="$ROTATE days ago"`.*"
    echo "Deleting stale backup files $STALE" >> $LOG
    rm -f $STALE
    rm -f $STALELOG
fi

chmod 0600 $BACKUPDIR/*

echo -n `date -R` >> $LOG
echo " MySQL backup procedure for $HOST completed" >> $LOG
