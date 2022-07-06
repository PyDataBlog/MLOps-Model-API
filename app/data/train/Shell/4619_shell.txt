#!/bin/bash
#Create user

username=$1
passwd=$2

#Create username /etc/passwd
if grep -q $username /etc/passwd; then
	sudo echo "User $username already exist in /etc/passwd !"
else
	sudo useradd -p $passwd -s /usr/bin/mysecureshell $username -g sftpusers
	sudo echo "User $username added in passwdwd,shadow and sftpusers group affected !"
	
	#changing to encrypted password
        sudo echo "$username:$passwd" | sudo chpasswd
	
	#Set quota Web
	#sudo setquota -u $username 204800 256000 204800 256000 /dev/sda5 
	#sudo echo "Quota sets for $username to 250Mo !"
fi
