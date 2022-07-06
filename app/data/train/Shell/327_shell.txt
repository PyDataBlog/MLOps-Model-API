#!/bin/bash

function logfun(){

	local MEX=$@
	echo $MEX	
	logger -s -i -t "randomly" -p user.info "The number is "${MEX}
}

logfun "Got a new random: "$RANDOM
logfun "Got a new random: "$RANDOM
logfun "Got a new random: "$RANDOM
