#!/bin/sh

# clean_old_bak
# Datum: 22.06.2011
# Autor: Computerwerkstatt Glocksee Hannover
# Version 0.1


# find loescht alle Dateien, die älter als x Tage sind

/usr/bin/find /pfad/zum/server_backup/ -mtime +5 -type f -exec /bin/rm "{}" \; &

exit $?
