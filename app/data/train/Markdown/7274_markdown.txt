1. SSH into boot2docker vm - created from virtualbox

   VBoxManage showvminfo <vm_name> | grep ssh
   ssh docker@<host_ip> -p <host_port>       # host ip would be max "localhost"
   use password - tcuser
   
   To switch to "root" user
   sudo -i   or   sudo su