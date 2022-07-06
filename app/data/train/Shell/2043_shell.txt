#!/bin/bash
################################################################################
# Script Name: ios-access.bash
# Author: Hugh Paterson III <email here>
# Version: 0.01
# License: GPL
# Dependencies: git, make. /github-tools folder
# Remaining work: 1) dependency check, 2) install folder correction 3) address photo install optoins.


sudo apt install libimobiledevice-utils
sudo apt-get install ideviceinstaller python-imobiledevice libimobiledevice-utils libimobiledevice4 libplist2 python-plist ifuse

git clone https://github.com/libimobiledevice/libplist.git github-tools
./github-tools/libplist/autogen.sh
make -C /github-tools/libplist
sudo make install -C /github-tools/libplist
git clone https://github.com/libimobiledevice/libusbmuxd.git
./github-tools/libusbmuxd/autogen.sh
make -C /github-tools/libusbmuxd
sudo make install -C /github-tools/libusbmuxd
#There was a way to view the device with ideviceinfo, some how I was able to register the device to the mahine and then things started working. Becky's phone did not go through the same stepts... Not sure what I did or didn't do.


#Make other users on the computer able to view the idevice by editing the /etc/fuse.conf file. Uncomment last line of the file.

#To also approach the photo shareing solution space. There is an app which uses Wi-Fi called: Photo Transfer App. This is available on chromium via the google store. It is avaible for android, and for iOS. This could be a rapid way to collect images.
