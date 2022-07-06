#!/bin/bash
# Script to add a user to Linux system
if [ $(id -u) -eq 0 ]; then
        read -p "Enter username : " username
        read -s -p "Enter password : " password
        egrep "^$username:" /etc/passwd >/dev/null
        if [ $? -eq 0 ]; then
                echo "$username exists!"
                exit 1
        else
                pass=$(perl -e 'print crypt($ARGV[0], "password")' $password)
                useradd -m -p $pass $username
                if [ $? -eq 0 ]; then
                    mkdir /var/log/httpd/$username
		            mkdir /home/$username/public_html
                    usermod -a -G apache $username
                    chmod 711 /home/$username
                    chmod 755 /home/$username/public_html
                    chown -R $username:apache /home/$username/public_html
                    mkdir /home/$username/.ssh
                    chmod 700 /home/$username/.ssh
                    cat /root/.ssh/id_rsa.pub > /home/$username/.ssh/authorized_keys
                    chmod 400 /home/$username/.ssh/authorized_keys
                    chown $username:$username -Rf /home/$username/.ssh
                    echo "User has been added to system!"
		else
                    echo "Failed to add a user!"
                fi
        fi
else
        echo "Only root may add a user to the system"
        exit 2
fi