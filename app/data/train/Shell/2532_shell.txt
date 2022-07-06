#!/bin/bash
UDIR="/root/"
FRDIR="yoursitename$(date '+%b%d%Y')"
HOME="/home/yourusername/"
sudo -u root mkdir $UDIR$FRDIR
cp -R /var/www/yoursitename/ $UDIR$FRDIR
cp -R /etc/nginx/ $UDIR$FRDIR
mysqldump yoursitesql -u root --opt > $UDIR$FRDIR/yoursitesql.sql
mysqldump mysql -u root --opt > $UDIR$FRDIR/mysql.sql
cp /etc/php5/fpm/pool.d/www.conf $UDIR$FRDIR
sed -i '1s"^";this file is /etc/php5/fpm/pool.d/www.conf\n"' $UDIR$FRDIR/www.conf
tar -zcvf $FRDIR.tar.gz  "$FRDIR"
cp $UDIR$FRDIR.tar.gz $HOME
rm -rf $UDIR$FRDIR $UDIR$FRDIR.tar.gz
