cmd_arch/arm/lib/getuser.o := /opt/arm-2009q3/bin/arm-none-eabi-gcc -Wp,-MD,arch/arm/lib/.getuser.o.d  -nostdinc -isystem /opt/arm-2009q3/bin/../lib/gcc/arm-none-eabi/4.4.1/include -I/home/sembre/android/GT-S5360_GB_Opensource_Update3/common/arch/arm/include -Iinclude  -I../modules/include -include include/generated/autoconf.h -D__KERNEL__ -mlittle-endian -Iarch/arm/mach-bcm215xx/include -Iarch/arm/plat-bcmap/include -D__ASSEMBLY__ -mabi=aapcs-linux -mno-thumb-interwork -funwind-tables  -D__LINUX_ARM_ARCH__=6 -march=armv6k -mtune=arm1136j-s -include asm/unified.h -msoft-float -gdwarf-2       -c -o arch/arm/lib/getuser.o arch/arm/lib/getuser.S

deps_arch/arm/lib/getuser.o := \
  arch/arm/lib/getuser.S \
    $(wildcard include/config/thumb2/kernel.h) \
  /home/sembre/android/GT-S5360_GB_Opensource_Update3/common/arch/arm/include/asm/unified.h \
    $(wildcard include/config/arm/asm/unified.h) \
  include/linux/linkage.h \
  include/linux/compiler.h \
    $(wildcard include/config/trace/branch/profiling.h) \
    $(wildcard include/config/profile/all/branches.h) \
    $(wildcard include/config/enable/must/check.h) \
    $(wildcard include/config/enable/warn/deprecated.h) \
  /home/sembre/android/GT-S5360_GB_Opensource_Update3/common/arch/arm/include/asm/linkage.h \
  /home/sembre/android/GT-S5360_GB_Opensource_Update3/common/arch/arm/include/asm/errno.h \
  include/asm-generic/errno.h \
  include/asm-generic/errno-base.h \

arch/arm/lib/getuser.o: $(deps_arch/arm/lib/getuser.o)

$(deps_arch/arm/lib/getuser.o):
