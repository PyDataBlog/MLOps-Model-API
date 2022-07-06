#!/bin/sh
###
### File   : install.sh
### Title  : DOTfiles install script
### Author : ɛntiˈtɛːt.kaɪ̯
### Date   : 2016-03-18
###



### Text attributes
	ATTB="$(tput bold)"
	ATT0="$(tput sgr0)"  # Reset all attributes

### Foreground colors
	FCRED="$(tput setaf 1)"
	FCGREEN="$(tput setaf 2)"

### Markup
	error()  { printf "${FCRED}${ATTB}Error:${ATT0} ${FCRED}$1${ATT0}\n"; }
	title()  { printf "${ATTB}$1${ATT0}\n"; }
	notice() { printf "    $1"; }
	check()  { printf " ${FCGREEN}✔${ATT0}\n"; }

### DOTfiles environment
	DFNAME=$(basename "$0")  # Name of this script
	DFPATH=$PWD              # Absolute path to this script



### vim:nowrap:noet:ts=4 sw=4 sts=4
