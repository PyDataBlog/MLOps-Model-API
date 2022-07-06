#!/bin/bash

# Post questions/issues to:
# https://github.com/Topani/JumpStartOS/issues

##**********************
## Change Log
## 
## Date        Agent
## ----        -----
## 01/27/2016  Topani
##**********************
# Summary:
# this script is a collection of commands to add the handbrake
# ppa and install handbrake-gtk and handbrake-cli which are the 
# handbrake gui and handbrake command line tool respectively.

# all commands are using the "--yes" switch to bypass Y/N checks

# handbrake release ppa
# handbrake snapshots (nightly) builds can be installed by adding
# ppa:stebbins/handbrake-git-snapshots instead.
sudo add-apt-repository ppa:stebbins/handbrake-releases --yes

sudo apt-get update --yes

sudo apt-get install handbrake-gtk --yes

sudo apt-get install handbrake-cli --yes


