#!/bin/bash

export ARCH=arm
export CROSS_COMPILE=~/AndroidKernels/toolchains/SaberModarm-eabi-4.9/bin/arm-eabi-
mkdir output

make VARIANT_DEFCONFIG=msm8974pro_sec_klte_vzw_defconfig msm8974_sec_defconfig SELINUX_DEFCONFIG=selinux_defconfig

make -j4
