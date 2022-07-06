#!/bin/bash

#Update system
sudo apt-get update
sudo apt-get upgrade
sudo rpi-upgrade

#Optional tools
sudo apt-get install vim tmux
sudo echo "syntax on" > /home/pi/.vimrc

#Force US keymap
sudo echo "setxkbmap us" > /home/pi/.xsessionrc



#Begin rise vision setup configuration
#Settings may change depending on your output device!
sudo sh -c "sed -i 's/#hdmi_drive=2/hdmi_drive=2/g' /boot/config.txt"
sudo sh -c "sed -i 's/overscan_left=24/#overscan_left=24/g' /boot/config.txt"
sudo sh -c "sed -i 's/overscan_right=24/#overscan_right=24/g' /boot/config.txt"
sudo sh -c "sed -i 's/overscan_top=16/#overscan_top=16/g' /boot/config.txt"
sudo sh -c "sed -i 's/overscan_bottom=20/#overscan_bottom=20/g' /boot/config.txt"
sudo sh -c "sed -i 's/disable_overscan=0/#disable_overscan=0/g' /boot/config.txt"
sudo sh -c "echo "hdmi_group=2" >> /boot/config.txt"
sudo sh -c "echo "hdmi_mode=82" >> /boot/config.txt"
sudo sh -c "echo "sdtv_aspect=3" >> /boot/config.txt"
sudo sh -c "echo "hdmi_force_hotplug=1" >> /boot/config.txt"
sudo sh -c "echo "config_hdmi_boost=4" >> /boot/config.txt"
sudo sh -c "echo '@xset s noblank' >> /etc/xdg/lxsession/LXDE/autostart"
sudo sh -c "echo '@xset s off' >> /etc/xdg/lxsession/LXDE/autostart"
sudo sh -c "echo '@xset -dpms' >> /etc/xdg/lxsession/LXDE/autostart"
sudo sh -c "echo '@xset s noblank' >> /etc/xdg/lxsession/LXDE-pi/autostart"
sudo sh -c "echo '@xset s off' >> /etc/xdg/lxsession/LXDE-pi/autostart"
sudo sh -c "echo '@xset -dpms' >> /etc/xdg/lxsession/LXDE-pi/autostart"

#BEGIN Raspberry Pi Rise Vision Player installer mod script by Ryan

wget "https://rvaserver2.appspot.com/player/download?os=lnx" -O rvpiplayer.sh


sudo chmod +x rvpiplayer.sh


./rvpiplayer.sh


#Should be finished with stock RVplayer download and installation - resuming Pi mod script and deleting stock rvplayer script


rm rvpiplayer.sh


#installing additional components


sudo apt-get install openjdk-7-jre chromium-browser unclutter


#Modification of chrome component path, the rest of the chrome rise plugins are ok, except flash, we just need  to use the ARM chromium binary


rm ~/rvplayer/chrome-linux/chrome


sudo cp /usr/bin/chromium ~/rvplayer/chrome-linux/chrome


sudo chown pi:pi ~/rvplayer/chrome-linux/chrome


#New launcher script construction


sudo echo "java -jar ~/rvplayer/RisePlayer.jar 


unclutter" > ~/rvplayer/newstartupscript.sh


sudo chmod +x ~/rvplayer/newstartupscript.sh


sudo chown pi:pi ~/rvplayer/newstartupscript.sh


#Desktop autostart section fixes


sudo rm ~/.config/autostart/rvplayer.desktop


sudo echo "[Desktop Entry] 


Type=Application


Exec=/home/pi/rvplayer/newstartupscript.sh 


Name=Ryan's RVplayer mod for Raspberry Pi" > ~/.config/autostart/pirvplayer.desktop


sudo chown pi:pi ~/.config/autostart/pirvplayer.desktop


sudo chmod +x ~/.config/autostart/pirvplayer.desktop


echo ..................


echo done!


echo Modifications for RVplayer for use on the Raspberry Pi should now be complete.


echo 


echo Upon boot to desktop your Rise display should start as a blank display and ask for your display ID


echo 


echo Please Reboot Now...


echo Enter your display ID when prompted after startup.
