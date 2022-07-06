#!/usr/bin/ipython

library_file = open('/srv/http/.config/cmus/lib.pl');
tracks = library_file.readlines()
for x in tracks:
    print('<li>' + x + '</li>')
