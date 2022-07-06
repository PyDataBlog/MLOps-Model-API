#!/bin/bash

export KBUILD_BUILD_USER=omkar

export KBUILD_BUILD_HOST=megatron007

###### defines ######
export CROSS_COMPILE=~/toolchain/bin/aarch64-linux-android-
export ARCH=arm64
export SUBARCH=arm64
local_dir=$PWD
defconfig=bullhead_defconfig
jobs=32

###### defines ######
echo '#############'
echo 'making clean'
echo '#############'
make clean
make mrproper
# clean the sources
rm -rf out                                                                 
# clean the output folder
echo '#############'
echo 'making defconfig'
echo '#############'
make $defconfig
echo '#############'
echo 'making zImage'
echo '#############'
time make -j$jobs
echo '#############'
echo 'copying files to ./out'
echo '#############'
echo ''                                               
# make dirs for zImage and modules
mkdir out
cp arch/arm64/boot/Image.gz-dtb out/Image.gz-dtb
# copy zImage
# Find and copy modules
cp -r out/* ~/anykernel/                                   
# copy zImage and modules to a my folder
echo 'done'
echo ''
if [ -a arch/arm64/boot/Image.gz-dtb ]; then
echo '#############'
echo 'Making Anykernel zip'
echo '#############'
echo ''
cd ~/anykernel/ 
mv Image.gz-dtb zImage-dtb
rm -rf .git
rm -rf README
zip -r MeGaByTe-kernel-r2.zip ./ -x *.zip *.gitignore *EMPTY_DIRECTORY
if [[ $1 = -d ]]; then
cp $zipname ~/anykernel/$zipname
echo "Copying $zipname to My Folder"
fi
cd $local_dir                                                              
# cd back to the old dir
echo ''
echo '#############'
echo 'build finished successfully'
echo '#############'
else
echo '#############'
echo 'build failed!'
echo '#############'
fi
