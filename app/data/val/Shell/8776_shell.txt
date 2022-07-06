#!/bin/bash

"""
                Copyright (C) 2014 BuildQt

BuildQt is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation.

BuildQt is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License Version 3 for more details.

You should have received a copy of the GNU General Public License
along with BuildQt. If not, see <http://www.gnu.org/licenses/>.

See project home page at: <https://github.com/Linuts/BuildQt>
"""

#Setup script
cd ..
APP=${PWD##*/}
cd BuildQt

#Make app
qmake -o src/Makefile ../$APP.pro
cd src
make -s

#Move app
if [ -f "../src/$APP" ]
then
	echo "Trying to move $APP to bin folder."
	mkdir -p ../bin
	mv -u $APP ../bin/$APP
else
	"Can't find $APP in src folder!"
fi

#Run App
if [ -f "../bin/$APP" ]
then
	cd ../bin
	echo "Running $APP..."
	./$APP
	echo "$APP was closed."
else
	echo "Can't find $APP in bin folder!"
fi
