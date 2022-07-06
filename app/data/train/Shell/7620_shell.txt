#!/bin/sh

insmod ~/pinmux_module.ko a=0x481408A0 v=0x01
rmmod pinmux_module.ko
insmod ~/pinmux_module.ko a=0x4814089C v=0x01
rmmod pinmux_module.ko
insmod ~/pinmux_module.ko a=0x4814083C v=0x04
rmmod pinmux_module.ko
insmod ~/pinmux_module.ko a=0x48140898 v=0x01
rmmod pinmux_module.ko
insmod ~/pinmux_module.ko a=0x481408A4 v=0x01
rmmod pinmux_module.ko
insmod ~/pinmux_module.ko a=0x48140924 v=0x20
rmmod pinmux_module.ko
insmod ~/pinmux_module.ko a=0x48140928 v=0x20
rmmod pinmux_module.ko

./i2c-8812 -w 0x1a 0x01 0x16d
./i2c-8812 -w 0x1a 0x03 0x89


~/bin/mem_rdwr.out --rd 0x481408A0
~/bin/mem_rdwr.out --rd 0x4814089C
~/bin/mem_rdwr.out --rd 0x4814083C
~/bin/mem_rdwr.out --rd 0x48140898
~/bin/mem_rdwr.out --rd 0x481408A4
~/bin/mem_rdwr.out --rd 0x48140924
~/bin/mem_rdwr.out --rd 0x48140928
./i2c-8812 -r 0x1a 0x00 80
