#!/bin/bash
#
# Create a deb package for the specified Git commit ID of this repo,
# defaulting to the current HEAD commit if none specified.
#
# Options:
# -c <Git ref>, e.g. commit ID, to use 

TIMESTAMP=`date +"%Y-%m-%d %H:%M:%S"`
PACKAGE_NAME=sentry
VERSION=6.4.4
PKG_DIR=${PACKAGE_NAME}_${VERSION}
VIRTUALENV=.venv
WHEEL_DIR=wheel


# Creating package folder
rsync -avr deb_build/* $PKG_DIR/

# install wheels
if [ -e "$VIRTUALENV" ]
then
    echo "Re-using existing virtualenv"
else
    virtualenv --no-site-packages $VIRTUALENV || { echo "Virtualenv failed"; exit -1; }
    rm -f md5.requirements.last
fi

. $VIRTUALENV/bin/activate
pip install distribute==0.7.3
pip install setuptools==1.3.2
pip install pip==1.4.1
pip install wheel==0.22.0

md5sum $PKG_dir/srv/sentry/requirements.txt > md5.requirements.new
diff md5.requirements.new md5.requirements.last
REQUIREMENTS_DIFF=$?

PIP_WHEEL="pip wheel --wheel-dir=$WHEEL_DIR --timeout 300"
PIP_INSTALL="pip install --use-wheel --no-index --find-links=$WHEEL_DIR"
mkdir -p $WHEEL_DIR
if [ "$REQUIREMENTS_DIFF" -ne 0 ]
then
    $PIP_WHEEL -r $PKG_DIR/srv/sentry/requirements.txt || { echo "pip failed (requirements)"; exit -1; }
    $PIP_INSTALL -r $PKG_DIR/srv/sentry/requirements.txt
    mv md5.requirements.new md5.requirements.last
fi
# create a requirements file from which to install
pip freeze > $PKG_DIR/srv/sentry/requirements.txt
rsync -avr wheel $PKG_DIR/srv/sentry/

# Now create the control file from the template in deb_build
SIZE=`du -c $PKG_DIR/srv | grep total | cut -f1`

# export any vars that are going to be used in templates processed
# by envsubst
export VERSION
export SIZE
export PACKAGE_NAME
export TIMESTAMP

cat deb_build/DEBIAN/control | envsubst > $PKG_DIR/DEBIAN/control

# Create the md5sums file
find $PKG_DIR/srv -type f -exec md5sum {} >> $PKG_DIR/DEBIAN/md5sums \;
sed -i s#$PKG_DIR/##g $PKG_DIR/DEBIAN/md5sums

# Create the package itself && remove temporary files
dpkg -b $PKG_DIR && rm -rf $PKG_DIR

echo ${PKG_DIR}.deb
