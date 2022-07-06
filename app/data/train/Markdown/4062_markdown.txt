HOW TO USE
========
The script has changed and there is no need to install apps.
All you have to do is download the connections scripts.<br />
  <strong>CentOS:</strong> https://raw.github.com/CajuCLC/slucheck/master/sluconnectioncentos.sh<br />
  <strong>Ubuntu:</strong> https://raw.github.com/CajuCLC/slucheck/master/sluconnectionubuntu.sh

You can also install running git clone https://github.com/CajuCLC/slucheck.git

1. When you run the script it will ask for SERVER name, this will be the txt file with all the information.
2. The script will ask for the server IP.
3. It will create a ssh-key file and copy to the remote host, it will ask the remote server password here.
4. Now it will do all connections and run the script on the remote server, it will ask SERVER name again, use same name.
5. This is the last part where the txt file is transfered and ssh-key and txt file is deleted from the remote server.
6. Open the $SERVER.txt file on your server/computer and find all the informations there.

Save it to the ticket and all done.

Script created by:<br />
1. Eric Cavalcanti - Technical Account Manager - LATAM - Brazil<br />
2. Orlando Gentil - Linux System Administrator II - LATAM - Brazil
