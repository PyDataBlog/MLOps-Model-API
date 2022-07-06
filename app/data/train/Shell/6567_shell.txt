#!/bin/bash

#Qey is a quantum random password generator based on the Australian National 
#University Quantum Random Generator. http://qrng.anu.edu.au/

#Copyright (C) 2015 edkalrio
#This program is free software: you can redistribute it and/or modify
#it under the terms of the GNU Affero General Public License as
#published by the Free Software Foundation, either version 3 of the
#License, or (at your option) any later version.

#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#GNU Affero General Public License for more details.

#You should have received a copy of the GNU Affero General Public License
#along with this program. If not, see http://www.gnu.org/licenses/.

#Why is it written in bash? --because I'm sysadmin, not a programmer.

# download file
wget -O tmp -q "https://qrng.anu.edu.au/RawChar.php"
# get the char stream | remove tag and underscores |cut it
grep -o '.*</td>' tmp | tr -d '</>_' | cut -c1-${1:-12}
# cleanup 
rm tmp
