FROM pataquets/autossh

ONBUILD ADD autossh.conf.d/* /etc/autossh.conf.d/
