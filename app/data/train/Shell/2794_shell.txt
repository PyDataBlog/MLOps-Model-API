PKGS="wajig gedit libreoffice chromium-browser gimp openshot scribus openssh-server vim"
TO_REMOVE="abiword gnumeric apport update-manager update-manager-core"

sudo apt-get -y install $PKGS
sudo apt-get -y remove $TO_REMOVE

#cp -r home/* /etc/skel/

mkdir -p /etc/skel/.config/lxsession/Lubuntu/
mkdir -p /etc/skel/Skrivebord
mkdir -p /etc/skel/Skabeloner
cp autostart /etc/skel/.config/lxsession/Lubuntu/
cp -b lightdm.conf /etc/lightdm/lightdm.conf

cp -r website/ /home/egeadmin
ln -s /home/egeadmin/website/index.html /etc/skel/Skrivebord/intro.html

cp templates/* /etc/skel/Skrivebord/

#Now that everything is in place, create the users. Remmeber to log in to elev_skel before elev
./create_users.sh
