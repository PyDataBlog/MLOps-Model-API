#
# Copyright (C) 2009-2013 OpenWrt.org
#
# This is free software, licensed under the GNU General Public License v2.
# See /LICENSE for more information.
#
include $(TOPDIR)/rules.mk

ARCH:=arm
BOARD:=cubox
BOARDNAME:=Cubox
FEATURES:=squashfs pci
CFLAGS:=-Os -pipe -march=armv7-a -mfpu=vfpv3-d16 -mfloat-abi=softfp
MAINTAINER:=Test <test@example.com>

LINUX_VERSION:=3.4.39

include $(INCLUDE_DIR)/target.mk

$(eval $(call BuildTarget))
