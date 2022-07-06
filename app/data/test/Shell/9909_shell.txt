#!/usr/bin/bash
# this is the copy
# verify user, then chmod files with extension .cgi
# by keith crowder
# date: 12-07-2008
# project complete	
# 
echo "please enter a username:"
read user
# verify user
if id $user | grep "^uid" > /dev/null
then
   echo "this user has permission, proceed"
else
   echo "this user does not have permission, access denied!"
exit
fi
# note options
   echo "executable or non-executable?"
   echo "to make files executable, choose option 1"
   echo "for non-executable, choose option 2"
# read input from command line
read option
# option 1
if [ $option = "1" ]; then
	chmod +x *.cgi
   echo "files are now executable"
# option 2 
elif [ $option = "2" ]; then
	chmod -x *.cgi
   echo "files are no longer executable"
else
   echo "you need to choose an option!"
exit
fi
#
# end of program
#
