cmd_arch/x86/boot/video-bios.o := gcc -Wp,-MD,arch/x86/boot/.video-bios.o.d  -nostdinc -isystem /usr/lib/gcc/i686-linux-gnu/4.4.7/include -I/home/madhwang/Development/linuxSource/linux-2.6.34.7/arch/x86/include -Iinclude  -include include/generated/autoconf.h -D__KERNEL__ -I/home/madhwang/Development/linuxSource/linux-2.6.34.7/arch/x86/include -Iinclude -include include/generated/autoconf.h -g -Os -D_SETUP -D__KERNEL__ -DDISABLE_BRANCH_PROFILING -Wall -Wstrict-prototypes -march=i386 -mregparm=3 -include /home/madhwang/Development/linuxSource/linux-2.6.34.7/arch/x86/boot/code16gcc.h -fno-strict-aliasing -fomit-frame-pointer -ffreestanding -fno-toplevel-reorder -fno-stack-protector -mpreferred-stack-boundary=2 -m32   -D"KBUILD_STR(s)=\#s" -D"KBUILD_BASENAME=KBUILD_STR(video_bios)"  -D"KBUILD_MODNAME=KBUILD_STR(video_bios)"  -c -o arch/x86/boot/.tmp_video-bios.o arch/x86/boot/video-bios.c

deps_arch/x86/boot/video-bios.o := \
  arch/x86/boot/video-bios.c \
  /home/madhwang/Development/linuxSource/linux-2.6.34.7/arch/x86/boot/code16gcc.h \
  arch/x86/boot/boot.h \
  /usr/lib/gcc/i686-linux-gnu/4.4.7/include/stdarg.h \
  include/linux/types.h \
    $(wildcard include/config/uid16.h) \
    $(wildcard include/config/lbdaf.h) \
    $(wildcard include/config/phys/addr/t/64bit.h) \
    $(wildcard include/config/64bit.h) \
  /home/madhwang/Development/linuxSource/linux-2.6.34.7/arch/x86/include/asm/types.h \
    $(wildcard include/config/x86/64.h) \
    $(wildcard include/config/highmem64g.h) \
  include/asm-generic/types.h \
  include/asm-generic/int-ll64.h \
  /home/madhwang/Development/linuxSource/linux-2.6.34.7/arch/x86/include/asm/bitsperlong.h \
  include/asm-generic/bitsperlong.h \
  include/linux/posix_types.h \
  include/linux/stddef.h \
  include/linux/compiler.h \
    $(wildcard include/config/trace/branch/profiling.h) \
    $(wildcard include/config/profile/all/branches.h) \
    $(wildcard include/config/enable/must/check.h) \
    $(wildcard include/config/enable/warn/deprecated.h) \
  include/linux/compiler-gcc.h \
    $(wildcard include/config/arch/supports/optimized/inlining.h) \
    $(wildcard include/config/optimize/inlining.h) \
  include/linux/compiler-gcc4.h \
  /home/madhwang/Development/linuxSource/linux-2.6.34.7/arch/x86/include/asm/posix_types.h \
    $(wildcard include/config/x86/32.h) \
  /home/madhwang/Development/linuxSource/linux-2.6.34.7/arch/x86/include/asm/posix_types_32.h \
  include/linux/edd.h \
  /home/madhwang/Development/linuxSource/linux-2.6.34.7/arch/x86/include/asm/boot.h \
    $(wildcard include/config/physical/start.h) \
    $(wildcard include/config/physical/align.h) \
    $(wildcard include/config/kernel/bzip2.h) \
  /home/madhwang/Development/linuxSource/linux-2.6.34.7/arch/x86/include/asm/pgtable_types.h \
    $(wildcard include/config/kmemcheck.h) \
    $(wildcard include/config/x86/pae.h) \
    $(wildcard include/config/compat/vdso.h) \
    $(wildcard include/config/proc/fs.h) \
  include/linux/const.h \
  /home/madhwang/Development/linuxSource/linux-2.6.34.7/arch/x86/include/asm/page_types.h \
  /home/madhwang/Development/linuxSource/linux-2.6.34.7/arch/x86/include/asm/page_32_types.h \
    $(wildcard include/config/highmem4g.h) \
    $(wildcard include/config/page/offset.h) \
    $(wildcard include/config/4kstacks.h) \
  /home/madhwang/Development/linuxSource/linux-2.6.34.7/arch/x86/include/asm/pgtable_32_types.h \
    $(wildcard include/config/highmem.h) \
  /home/madhwang/Development/linuxSource/linux-2.6.34.7/arch/x86/include/asm/pgtable-2level_types.h \
  include/asm-generic/pgtable-nopud.h \
  include/asm-generic/pgtable-nopmd.h \
  /home/madhwang/Development/linuxSource/linux-2.6.34.7/arch/x86/include/asm/setup.h \
    $(wildcard include/config/x86/visws.h) \
    $(wildcard include/config/x86/mrst.h) \
  include/linux/pfn.h \
  /home/madhwang/Development/linuxSource/linux-2.6.34.7/arch/x86/include/asm/bootparam.h \
  include/linux/screen_info.h \
  include/linux/apm_bios.h \
  include/linux/ioctl.h \
  /home/madhwang/Development/linuxSource/linux-2.6.34.7/arch/x86/include/asm/ioctl.h \
  include/asm-generic/ioctl.h \
  /home/madhwang/Development/linuxSource/linux-2.6.34.7/arch/x86/include/asm/e820.h \
    $(wildcard include/config/nodes/shift.h) \
    $(wildcard include/config/efi.h) \
    $(wildcard include/config/hibernation.h) \
    $(wildcard include/config/memtest.h) \
  include/linux/early_res.h \
  include/linux/range.h \
  include/linux/ioport.h \
  /home/madhwang/Development/linuxSource/linux-2.6.34.7/arch/x86/include/asm/ist.h \
  include/video/edid.h \
    $(wildcard include/config/x86.h) \
  /home/madhwang/Development/linuxSource/linux-2.6.34.7/arch/x86/include/asm/x86_init.h \
  arch/x86/boot/bitops.h \
  /home/madhwang/Development/linuxSource/linux-2.6.34.7/arch/x86/include/asm/cpufeature.h \
    $(wildcard include/config/x86/invlpg.h) \
  /home/madhwang/Development/linuxSource/linux-2.6.34.7/arch/x86/include/asm/required-features.h \
    $(wildcard include/config/x86/minimum/cpu/family.h) \
    $(wildcard include/config/math/emulation.h) \
    $(wildcard include/config/x86/cmpxchg64.h) \
    $(wildcard include/config/x86/cmov.h) \
    $(wildcard include/config/x86/use/3dnow.h) \
    $(wildcard include/config/x86/p6/nop.h) \
    $(wildcard include/config/paravirt.h) \
  include/linux/bitops.h \
    $(wildcard include/config/generic/find/first/bit.h) \
    $(wildcard include/config/generic/find/last/bit.h) \
    $(wildcard include/config/generic/find/next/bit.h) \
  /home/madhwang/Development/linuxSource/linux-2.6.34.7/arch/x86/include/asm/processor-flags.h \
    $(wildcard include/config/vm86.h) \
  arch/x86/boot/video.h \

arch/x86/boot/video-bios.o: $(deps_arch/x86/boot/video-bios.o)

$(deps_arch/x86/boot/video-bios.o):
