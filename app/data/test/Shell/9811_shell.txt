#!/bin/bash

# Example form
# for i in $(); do
#       $i
# done

for user in $( cat /etc/passwd | awk -F ":" '{print $1}' ); do
	echo "There is a user called $user"
	echo "2nd line for $user"
done
~                                                                               
~                          
