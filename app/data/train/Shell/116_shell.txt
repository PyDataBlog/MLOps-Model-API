#!/bin/bash
#Coded by:FadyHazem/V2.0 Support:LinuxMint-Ubuntu-Debian.
#You can recodeing this script but yo must typing the own of this script(FadyHazem).
echo "Welcome i will help you to update your system."
sleep 4
echo "This script support:LinuxMint-Ubuntu-Debian"
sleep 2
read -p "What's your distro name?:" DISTRO
echo "Now updateing $DISTRO"
sleep 3
echo "Now loading..."
sleep 4
sudo apt-get update && sudo apt-get upgrade
