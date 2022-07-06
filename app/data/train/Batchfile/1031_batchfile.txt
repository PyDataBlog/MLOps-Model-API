cmd_lib/zlib_inflate/inffast.o := ppc_4xx-gcc -m32 -Wp,-MD,lib/zlib_inflate/.inffast.o.d  -nostdinc -isystem /tools/dev/eldk/usr/bin/../lib/gcc/powerpc-linux/4.2.2/include -I/tools/dev/repos/linux-2.6-xlnx/arch/powerpc/include -Iinclude  -include include/generated/autoconf.h -D__KERNEL__ -Iarch/powerpc -Wall -Wundef -Wstrict-prototypes -Wno-trigraphs -fno-strict-aliasing -fno-common -Werror-implicit-function-declaration -Wno-format-security -fno-delete-null-pointer-checks -O2 -msoft-float -pipe -Iarch/powerpc -ffixed-r2 -mmultiple -mno-altivec -mno-spe -mspe=no -funit-at-a-time -mno-string -Wa,-m405 -fno-stack-protector -fomit-frame-pointer -g -Wdeclaration-after-statement -Wno-pointer-sign -fno-strict-overflow   -D"KBUILD_STR(s)=\#s" -D"KBUILD_BASENAME=KBUILD_STR(inffast)"  -D"KBUILD_MODNAME=KBUILD_STR(zlib_inflate)"  -c -o lib/zlib_inflate/.tmp_inffast.o lib/zlib_inflate/inffast.c

deps_lib/zlib_inflate/inffast.o := \
  lib/zlib_inflate/inffast.c \
    $(wildcard include/config/have/efficient/unaligned/access.h) \
  include/linux/zutil.h \
  include/linux/zlib.h \
  include/linux/zconf.h \
  include/linux/string.h \
    $(wildcard include/config/binary/printf.h) \
  include/linux/compiler.h \
    $(wildcard include/config/trace/branch/profiling.h) \
    $(wildcard include/config/profile/all/branches.h) \
    $(wildcard include/config/enable/must/check.h) \
    $(wildcard include/config/enable/warn/deprecated.h) \
  include/linux/compiler-gcc.h \
    $(wildcard include/config/arch/supports/optimized/inlining.h) \
    $(wildcard include/config/optimize/inlining.h) \
  include/linux/compiler-gcc4.h \
  include/linux/types.h \
    $(wildcard include/config/uid16.h) \
    $(wildcard include/config/lbdaf.h) \
    $(wildcard include/config/phys/addr/t/64bit.h) \
    $(wildcard include/config/64bit.h) \
  /tools/dev/repos/linux-2.6-xlnx/arch/powerpc/include/asm/types.h \
    $(wildcard include/config/phys/64bit.h) \
  include/asm-generic/int-ll64.h \
  /tools/dev/repos/linux-2.6-xlnx/arch/powerpc/include/asm/bitsperlong.h \
  include/asm-generic/bitsperlong.h \
  include/linux/posix_types.h \
  include/linux/stddef.h \
  /tools/dev/repos/linux-2.6-xlnx/arch/powerpc/include/asm/posix_types.h \
  /tools/dev/eldk/usr/bin/../lib/gcc/powerpc-linux/4.2.2/include/stdarg.h \
  /tools/dev/repos/linux-2.6-xlnx/arch/powerpc/include/asm/string.h \
  include/linux/kernel.h \
    $(wildcard include/config/preempt/voluntary.h) \
    $(wildcard include/config/debug/spinlock/sleep.h) \
    $(wildcard include/config/prove/locking.h) \
    $(wildcard include/config/printk.h) \
    $(wildcard include/config/dynamic/debug.h) \
    $(wildcard include/config/ring/buffer.h) \
    $(wildcard include/config/tracing.h) \
    $(wildcard include/config/numa.h) \
    $(wildcard include/config/ftrace/mcount/record.h) \
  include/linux/linkage.h \
  /tools/dev/repos/linux-2.6-xlnx/arch/powerpc/include/asm/linkage.h \
  include/linux/bitops.h \
    $(wildcard include/config/generic/find/first/bit.h) \
    $(wildcard include/config/generic/find/last/bit.h) \
    $(wildcard include/config/generic/find/next/bit.h) \
  /tools/dev/repos/linux-2.6-xlnx/arch/powerpc/include/asm/bitops.h \
  /tools/dev/repos/linux-2.6-xlnx/arch/powerpc/include/asm/asm-compat.h \
    $(wildcard include/config/power4/only.h) \
    $(wildcard include/config/ibm405/err77.h) \
  /tools/dev/repos/linux-2.6-xlnx/arch/powerpc/include/asm/synch.h \
    $(wildcard include/config/ppc/e500mc.h) \
    $(wildcard include/config/e500.h) \
    $(wildcard include/config/smp.h) \
  include/linux/stringify.h \
  /tools/dev/repos/linux-2.6-xlnx/arch/powerpc/include/asm/feature-fixups.h \
    $(wildcard include/config/ppc64.h) \
  include/asm-generic/bitops/non-atomic.h \
  include/asm-generic/bitops/fls64.h \
  include/asm-generic/bitops/hweight.h \
  include/asm-generic/bitops/find.h \
  include/asm-generic/bitops/sched.h \
  include/linux/log2.h \
    $(wildcard include/config/arch/has/ilog2/u32.h) \
    $(wildcard include/config/arch/has/ilog2/u64.h) \
  include/linux/typecheck.h \
  include/linux/dynamic_debug.h \
  /tools/dev/repos/linux-2.6-xlnx/arch/powerpc/include/asm/byteorder.h \
  include/linux/byteorder/big_endian.h \
  include/linux/swab.h \
  /tools/dev/repos/linux-2.6-xlnx/arch/powerpc/include/asm/swab.h \
  include/linux/byteorder/generic.h \
  /tools/dev/repos/linux-2.6-xlnx/arch/powerpc/include/asm/bug.h \
    $(wildcard include/config/bug.h) \
    $(wildcard include/config/debug/bugverbose.h) \
  include/asm-generic/bug.h \
    $(wildcard include/config/generic/bug.h) \
    $(wildcard include/config/generic/bug/relative/pointers.h) \
  /tools/dev/repos/linux-2.6-xlnx/arch/powerpc/include/asm/div64.h \
  include/asm-generic/div64.h \
  lib/zlib_inflate/inftrees.h \
  lib/zlib_inflate/inflate.h \
  lib/zlib_inflate/inffast.h \
  /tools/dev/repos/linux-2.6-xlnx/arch/powerpc/include/asm/unaligned.h \
  include/linux/unaligned/access_ok.h \
  include/linux/unaligned/generic.h \

lib/zlib_inflate/inffast.o: $(deps_lib/zlib_inflate/inffast.o)

$(deps_lib/zlib_inflate/inffast.o):
