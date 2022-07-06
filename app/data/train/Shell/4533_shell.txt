#!/bin/bash

while true
do
	ctrl=$(cat /var/www/html/pibot/command)
	./control-codes/shell/control.sh ${ctrl}
done
