#!/bin/bash
sleep 15
env
cat /etc/hosts
supervisord -n -c /etc/supervisor/supervisord.conf
