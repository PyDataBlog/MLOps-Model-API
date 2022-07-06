cmd_arch/x86/kernel/cpu/feature_names.o := gcc -Wp,-MD,arch/x86/kernel/cpu/.feature_names.o.d  -nostdinc -isystem /usr/lib/gcc/x86_64-linux-gnu/4.8/include -D__KERNEL__ -Iinclude  -include include/linux/autoconf.h -Wall -Wundef -Wstrict-prototypes -Wno-trigraphs -fno-strict-aliasing -fno-common -Werror-implicit-function-declaration -Os -Wframe-larger-than=2048  -fno-stack-protector -m32 -msoft-float -mregparm=3 -freg-struct-return -mpreferred-stack-boundary=2  -march=i686 -ffreestanding   -pipe -Wno-sign-compare -fno-asynchronous-unwind-tables -mno-sse -mno-mmx -mno-sse2 -mno-3dnow -Iinclude/asm-x86/mach-default -fomit-frame-pointer -Wdeclaration-after-statement -Wno-pointer-sign     -D"KBUILD_STR(s)=\#s" -D"KBUILD_BASENAME=KBUILD_STR(feature_names)"  -D"KBUILD_MODNAME=KBUILD_STR(feature_names)" -c -o arch/x86/kernel/cpu/feature_names.o arch/x86/kernel/cpu/feature_names.c

deps_arch/x86/kernel/cpu/feature_names.o := \
  arch/x86/kernel/cpu/feature_names.c \
  include/asm/cpufeature.h \
    $(wildcard include/config/x86/invlpg.h) \
    $(wildcard include/config/x86/64.h) \
  include/asm/required-features.h \
    $(wildcard include/config/x86/minimum/cpu/family.h) \
    $(wildcard include/config/math/emulation.h) \
    $(wildcard include/config/x86/pae.h) \
    $(wildcard include/config/x86/cmov.h) \
    $(wildcard include/config/x86/use/3dnow.h) \
  include/linux/bitops.h \
    $(wildcard include/config/generic/find/first/bit.h) \
    $(wildcard include/config/generic/find/next/bit.h) \
  include/asm/types.h \
    $(wildcard include/config/x86/32.h) \
    $(wildcard include/config/highmem64g.h) \
  include/asm-generic/int-ll64.h \
  include/asm/bitops.h \
  include/linux/compiler.h \
    $(wildcard include/config/enable/must/check.h) \
    $(wildcard include/config/enable/warn/deprecated.h) \
  include/linux/compiler-gcc4.h \
  include/linux/compiler-gcc.h \
    $(wildcard include/config/arch/supports/optimized/inlining.h) \
    $(wildcard include/config/optimize/inlining.h) \
  include/asm/alternative.h \
    $(wildcard include/config/smp.h) \
    $(wildcard include/config/paravirt.h) \
  include/linux/types.h \
    $(wildcard include/config/uid16.h) \
    $(wildcard include/config/lbd.h) \
    $(wildcard include/config/lsf.h) \
    $(wildcard include/config/resources/64bit.h) \
  include/linux/posix_types.h \
  include/linux/stddef.h \
  include/asm/posix_types.h \
  include/asm/posix_types_32.h \
  include/asm/asm.h \
  include/asm-generic/bitops/sched.h \
  include/asm-generic/bitops/hweight.h \
  include/asm-generic/bitops/fls64.h \
  include/asm-generic/bitops/ext2-non-atomic.h \
  include/asm-generic/bitops/le.h \
  include/asm/byteorder.h \
    $(wildcard include/config/x86/bswap.h) \
  include/linux/byteorder/little_endian.h \
  include/linux/byteorder/swab.h \
  include/linux/byteorder/generic.h \
  include/asm-generic/bitops/minix.h \

arch/x86/kernel/cpu/feature_names.o: $(deps_arch/x86/kernel/cpu/feature_names.o)

$(deps_arch/x86/kernel/cpu/feature_names.o):
