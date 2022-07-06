#!/bin/bash
###########################################################################
# eclipse-installer will install Eclipse with Java, PHP, Python, C++,  
# Ruby, and ShellEd on Ubuntu and MacOS. It will also setup the Eclipse 
# launch script to move OpenJDK to the /run/shm RAM Disk for 
# Ubuntu.
#
# Copyright (C) 2016 Chad Sutton <casutton@noctrl.edu>
# This program is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the 
# Free Software Foundation, either version 3 of the License, or (at your 
# option) any later version.
# This program is distributed in the hope that it will be useful, but 
# WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
# See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along 
# with this program. If not, see http://www.gnu.org/licenses/.
###########################################################################


##### Functions
osis()
{
    local n=0
    if [[ "$1" = "-n" ]]; then n=1;shift; fi

    # echo $OS|grep $1 -i >/dev/null
    uname -s |grep -i "$1" >/dev/null

    return $(( $n ^ $? ))
}


##### Setup variables for the OS specific install (Linux or Mac OS X)
eclipsever="7"
#ampstackver="5.6.19-0"
# download example
# http://www.eclipse.org/downloads/download.php?file=/technology/epp/downloads/release/neon/3/eclipse-php-neon-3-linux-gtk-x86_64.tar.gz
# http://www.eclipse.org/downloads/download.php?file=/technology/epp/downloads/release/mars/2/eclipse-php-mars-2-linux-gtk-x86_64.tar.gz\&r=1
# https://www.eclipse.org/downloads/download.php?file=/technology/epp/downloads/release/oxygen/R/eclipse-php-oxygen-R-linux-gtk-x86_64.tar.gz&mirror_id=1135
# https://www.eclipse.org/downloads/download.php?file=/technology/epp/downloads/release/2019-06/R/eclipse-jee-2019-06-R-macosx-cocoa-x86_64.dmg

eclipseurl="http://www.eclipse.org/downloads/download.php?file=/technology/epp/downloads/release/"
eclipserel="2019-06"
eclipserelver="R"

# Is this Mac or Linux
osis Darwin &&
{
	ostype="Darwin"
	installroot="/Applications"
	eclipsedlfile="eclipse-php-${eclipserel}-${eclipserelver}-macosx-cocoa-x86_64.dmg" 
	eclipsedlurl="${eclipseurl}/${eclipserel}/${eclipserelver}/${eclipsedlfile}&r=1"
	eclipsedirname="Eclipse.app"
	eclipseloc="$installroot/$eclipsedirname"
	eclipsebin="$installroot/$eclipsedirname/Contents/MacOS/eclipse"
	#ampstackloc="$installroot/mampstack"
	#ampstackfile="bitnami-mampstack-${ampstackver}-osx-x86_64-installer.dmg"
	#ampstackurl="https://downloads.bitnami.com/files/stacks/mampstack/${ampstackver}/${ampstackfile}"
}

osis Linux &&
{
    ostype="Linux"
    installroot="/usr/local"
    eclipsedlfile="eclipse-php-${eclipserel}-${eclipserelver}-linux-gtk-x86_64.tar.gz" 
    eclipsedlurl="${eclipseurl}/${eclipserel}/${eclipserelver}/${eclipsedlfile}&r=1"
    eclipsedirname="eclipse"
    eclipseloc="$installroot/$eclipsedirname"
    eclipsebin="$installroot/$eclipsedirname/eclipse"
    #ampstackloc="$installroot/lampstack"
    #ampstackfile="bitnami-lampstack-${ampstackver}-linux-64-installer.run"
    #https://bitnami.com/redirect/to/97711/bitnami-lampstack-5.6.19-0-linux-x64-installer.run
    #ampstackurl="https://downloads.bitnami.com/files/stacks/lampstack/${ampstackver}/${ampstackfile}"
}


##### Download Eclipse SDK
echo Installing Eclipse with PHP, Python, C++, Ruby, and ShellEd on Ubuntu.
echo Operating system is ${ostype}
echo Installation Root is $installroot
echo Eclipse download file is $eclipsedlfile
echo Eclipse install location is $eclipseloc
echo Eclipse download URL is $eclipsedlurl


if [ -e /tmp/$eclipsedlfile ]
	then
	rm /tmp/$eclipsedlfile 
fi

osis Linux &&
{
	echo Downloading $eclipsedlfile
	wget -O /tmp/$eclipsedlfile "${eclipsedlurl}"
}

osis Darwin &&
{
	echo Downloading $eclipsedlfile
	cd /tmp
	curl -L -o $eclipsedlfile "${eclipsedlurl}"
	cd -
}

# Clean up any previous tar file extractions
if [ -d /tmp/$eclipsedirname ] 
        then
        echo Removing /tmp/$eclipsedirname
        rm -R /tmp/$eclipsedirname
fi


osis Linux &&
{
	if tar xzf  /tmp/$eclipsedlfile -C /tmp > /dev/null 2>&1;then
		echo Successfully extracted $eclipsedlfile
	else
		echo "Error extracting /tmp/$eclipsedlfile! Aborting." 1>&2
		exit 1
	fi
}

osis Darwin &&
{
	echo Mounting Eclipse DMG volume
	hdiutil attach -nobrowse -quiet -mountpoint /Volumes/eclipse /tmp/$eclipsedlfile
}

# Remove previous eclipse directory
if [ -d $eclipseloc ] 
	then
	echo Removing $eclipseloc
	sudo rm -r $eclipseloc
fi

# Install new Eclipse
echo Copying install files for Eclipse
osis Linux &&
{
	sudo mv /tmp/$eclipsedirname $installroot/ 2>&1
}

osis Darwin &&
{
	sudo cp -a /Volumes/eclipse/Eclipse.app $installroot/ 2>&1
}

osis Darwin &&
{
Echo Updating ownership rights for $eclipseloc
sudo chown -R root:admin $eclipseloc 2>&1
}

osis Linux &&
{
echo Updating ownership rights for $eclipseloc
sudo chown -R root:root $eclipseloc 2>&1
}

sudo chmod -R 755 $eclipseloc 2>&1

echo Installing Eclipse Plugins for C++, PHP, Java, and Python
sudo $eclipsebin -nosplash \
  -application org.eclipse.equinox.p2.director \
  -repository http://download.eclipse.org/releases/${eclipserel}/,http://download.eclipse.org/tools/cdt/releases/${eclipserel}/ \
  -destination $eclipseloc \
  -installIU org.eclipse.cdt.feature.group \
  -installIU org.eclipse.dltk.ruby.feature.group \
  -installIU org.eclipse.php.feature.group \
  2>&1
  #-installIU org.eclipse.dltk.python.feature.group

# Install Python Plugin
sudo $eclipsebin -nosplash \
  -application org.eclipse.equinox.p2.director \
  -repository http://pydev.org/updates/ \
  -destination $eclipseloc \
  -installIU org.python.pydev.feature.feature.group \
  2>&1

# Install LinuxTools
sudo $eclipsebin -nosplash \
  -application org.eclipse.equinox.p2.director \
  -repository http://download.eclipse.org/linuxtools/update \
  -destination $eclipseloc \
  -installIU org.eclipse.linuxtools.binutils \
  -installIU org.eclipse.linuxtools.man.help \
  -installIU org.eclipse.linuxtools.man.core \
  2>&1

# Install ShellEd Plugin
sudo $eclipsebin -nosplash \
  -application org.eclipse.equinox.p2.director \
  -repository http://download.eclipse.org/technology/dltk/updates-dev/latest/ \
  -destination $eclipseloc \
  -installIU org.eclipse.dltk.core \
  -installIU org.eclipse.dltk.sh.feature.group \
  2>&1

sudo $eclipsebin -nosplash \
  -application org.eclipse.equinox.p2.director \
  -repository http://download.eclipse.org/technology/dltk/updates-dev/latest/ \
  -destination $eclipseloc \
  -installIU org.eclipse.dltk.core \
  -installIU org.eclipse.dltk.sh.sdk.feature.group \
  2>&1


# Install Drupal Plugin
sudo $eclipsebin -nosplash \
  -application org.eclipse.equinox.p2.director \
  -repository http://xtnd.us/downloads/eclipse \
  -destination $eclipseloc \
  -installIU us.xtnd.eclipse.pdt.plugin.drupal \
  2>&1


# Create launch script and desktop icon
osis Linux &&
{
eclipse_bin=$(cat <<EOF
#!/bin/sh
export ECLIPSE_HOME='${installroot}/eclipse'
cp -r /usr/lib/jvm/java-11-openjdk-amd64 /run/shm
\$ECLIPSE_HOME/eclipse \$*
EOF
)

eclipse_icon=$(cat <<EOF
[Desktop Entry]
Encoding=UTF-8
Name=Eclipse
Comment=Eclipse IDE
Exec=eclipse
Icon=${installroot}/eclipse/icon.xpm
Terminal=false
Type=Application
Categories=GNOME;Application;Development;
StartupNotify=true
EOF
)

eclipse_inivm=$(cat <<EOF
-vm
/run/shm/java-11-openjdk-amd64/bin/java
EOF
)

# -XX:MaxPermSize=512m

eclipse_ini=$(cat <<EOF
-XX:+AggressiveOpts
-XX:PermSize=512m
-Xms2048m
-Xmx2048m
-Xmn512m
-Xss2m
EOF
)

sudo touch /usr/bin/eclipse
sudo chmod 755 /usr/bin/eclipse
echo -e "${eclipse_bin}" | sudo tee /usr/bin/eclipse
echo -e "${eclipse_icon}" | sudo tee /usr/share/applications/eclipse.desktop
sudo mv ${installroot}/eclipse/eclipse.ini ${installroot}/eclipse/eclipse.ini.org
echo -e "${eclipse_inivm}" |tee /tmp/eclipsevm.ini
echo -e "${eclipse_ini}" |tee /tmp/eclipse.ini
cat /tmp/eclipsevm.ini ${installroot}/eclipse/eclipse.ini.org /tmp/eclipse.ini | sudo tee ${installroot}/eclipse/eclipse.ini
sudo sed --in-place "/-Xms256m/d" ${installroot}/eclipse/eclipse.ini
sudo sed --in-place "/-Xmx1024m/d" ${installroot}/eclipse/eclipse.ini
}

#-XX:MaxPermSize=512m
osis Darwin &&
{
eclipse_ini=$(cat <<EOF
-XX:+AggressiveOpts
-XX:PermSize=512m
-Xms2048m
-Xmx2048m
-Xmn512m
-Xss2m
EOF
)

sudo cp ${eclipseloc}/Contents/Eclipse/eclipse.ini ${eclipseloc}/Contents/Eclipse/eclipse.ini.org
sudo rm ${eclipseloc}/Contents/Eclipse/eclipse.ini
echo -e "${eclipse_ini}" |tee /tmp/eclipse.ini
cat ${eclipseloc}/Contents/Eclipse/eclipse.ini.org /tmp/eclipse.ini | sudo tee ${eclipseloc}/Contents/Eclipse/eclipse.ini 
sudo sed -i '' "/-Xms256m/d" ${eclipseloc}/Contents/Eclipse/eclipse.ini
sudo sed -i '' "/-Xmx1024m/d" ${eclipseloc}/Contents/Eclipse/eclipse.ini
}

#cleanup
echo Cleaning up download tmp files

osis Darwin &&
{
	echo Unmounting DMG volume
	hdiutil detach -quiet /Volumes/eclipse
}

if [ -e /tmp/$eclipsedlfile ]
        then
        rm /tmp/$eclipsedlfile 
fi

if [ -d /tmp/eclipse ] 
        then
        echo Removing /tmp/eclipse
        rm -R /tmp/eclipse
fi


