TCHAIN=~/android/gcc-linaro-arm-linux-gnueabihf-4.7/bin/arm-linux-gnueabihf-
WiFiPATCH=drivers/net/wireless/compat-wireless_R5.SP2.03
MTARGET=~/android/kernel/BUILD/modules

cp -f ./arch/arm/configs/ap33_android_mpdec ./arch/arm/configs/ap33_android_defconfig

make ARCH=arm CROSS_COMPILE=$TCHAIN ap33_android_defconfig
date '+%Y%m%d%H' > .version

make ARCH=arm CROSS_COMPILE=$TCHAIN -j4
make ARCH=arm CROSS_COMPILE=$TCHAIN -C $WiFiPATCH KLIB=`pwd` KLIB_BUILD=`pwd` -j2

for x in `find . | grep \.ko$` ; do
	echo "> $x"
	cp -f $x $MTARGET
done
for x in `find $WiFiPATCH | grep \.ko$` ; do
	echo "> $x"
	cp -f $x $MTARGET
done
cp ./arch/arm/boot/zImage $MTARGET/../zImage

ls -l $MTARGET
ls -l $MTARGET/../zImage
