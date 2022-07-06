cmd_arch/x86/boot/version.o := gcc -Wp,-MD,arch/x86/boot/.version.o.d  -nostdinc -isystem /usr/lib/gcc/x86_64-linux-gnu/4.8/include -D__KERNEL__ -Iinclude  -include include/linux/autoconf.h -Iinclude  -include include/linux/autoconf.h -g -Os -D_SETUP -D__KERNEL__ -Wall -Wstrict-prototypes -march=i386 -mregparm=3 -include /home/nhong/workspace/kernel-tut/arch/x86/boot/code16gcc.h -fno-strict-aliasing -fomit-frame-pointer  -ffreestanding  -fno-toplevel-reorder  -fno-stack-protector  -mpreferred-stack-boundary=2 -m32 -D__BIG_KERNEL__   -D"KBUILD_STR(s)=\#s" -D"KBUILD_BASENAME=KBUILD_STR(version)"  -D"KBUILD_MODNAME=KBUILD_STR(version)" -c -o arch/x86/boot/version.o arch/x86/boot/version.c

deps_arch/x86/boot/version.o := \
  arch/x86/boot/version.c \
  /home/nhong/workspace/kernel-tut/arch/x86/boot/code16gcc.h \
  arch/x86/boot/boot.h \
  /usr/lib/gcc/x86_64-linux-gnu/4.8/include/stdarg.h \
  include/linux/types.h \
    $(wildcard include/config/uid16.h) \
    $(wildcard include/config/lbd.h) \
    $(wildcard include/config/lsf.h) \
    $(wildcard include/config/resources/64bit.h) \
  include/linux/posix_types.h \
  include/linux/stddef.h \
  include/linux/compiler.h \
    $(wildcard include/config/enable/must/check.h) \
    $(wildcard include/config/enable/warn/deprecated.h) \
  include/linux/compiler-gcc4.h \
  include/linux/compiler-gcc.h \
    $(wildcard include/config/arch/supports/optimized/inlining.h) \
    $(wildcard include/config/optimize/inlining.h) \
  include/asm/posix_types.h \
    $(wildcard include/config/x86/32.h) \
  include/asm/posix_types_32.h \
  include/asm/types.h \
    $(wildcard include/config/x86/64.h) \
    $(wildcard include/config/highmem64g.h) \
  include/asm-generic/int-ll64.h \
  include/linux/edd.h \
  include/asm/boot.h \
    $(wildcard include/config/physical/start.h) \
    $(wildcard include/config/physical/align.h) \
  include/asm/setup.h \
    $(wildcard include/config/paravirt.h) \
  include/linux/pfn.h \
  include/asm/bootparam.h \
  include/linux/screen_info.h \
  include/linux/apm_bios.h \
  include/linux/ioctl.h \
  include/asm/ioctl.h \
  include/asm-generic/ioctl.h \
  include/asm/e820.h \
  include/asm/e820_32.h \
    $(wildcard include/config/pm.h) \
    $(wildcard include/config/hibernation.h) \
  include/linux/ioport.h \
  include/asm/ist.h \
  include/video/edid.h \
    $(wildcard include/config/x86.h) \
  include/linux/utsrelease.h \
  include/linux/compile.h \

arch/x86/boot/version.o: $(deps_arch/x86/boot/version.o)

$(deps_arch/x86/boot/version.o):
