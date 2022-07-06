sed -i "s/^.*requiretty/#Defaults requiretty/" /etc/sudoers

cat > /etc/yum.repos.d/epel.repo << EOM
[epel]
name=epel
baseurl=http://ftp.riken.jp/Linux/fedora/epel/6/\$basearch/
enabled=1
gpgcheck=0
EOM

yum -y install gcc make gcc-c++ kernel-devel-`uname -r` zlib-devel openssl-devel readline-devel perl wget dkms nfs-utils --releasever=6.4

# Make ssh faster by not waiting on DNS
#echo "UseDNS no" >> /etc/ssh/sshd_config
