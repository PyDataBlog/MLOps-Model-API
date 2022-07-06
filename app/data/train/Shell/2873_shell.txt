#!/bin/bash
#!/bin/bash

export ARCH=arm
export CROSS_COMPILE=/home/williamkosasih/arm-eab4.7/bin/arm-eabi-
mkdir output

make -C $(pwd) O=output VARIANT_DEFCONFIG=msm8226-sec_millet3g_defconfig msm8226-sec_defconfig SELINUX_DEFCONFIG=selinux_defconfig
make -C $(pwd) O=output

cp output/arch/arm/boot/zImage $(pwd)/lazyflash/zImage
cd lazyflash
make
