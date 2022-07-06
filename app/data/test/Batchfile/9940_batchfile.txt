cmd_arch/powerpc/lib/copy_32.o := ppc_4xx-gcc -m32 -Wp,-MD,arch/powerpc/lib/.copy_32.o.d  -nostdinc -isystem /tools/dev/eldk/usr/bin/../lib/gcc/powerpc-linux/4.2.2/include -I/tools/dev/repos/linux-2.6-xlnx/arch/powerpc/include -Iinclude  -include include/generated/autoconf.h -D__KERNEL__ -Iarch/powerpc -D__ASSEMBLY__ -Iarch/powerpc -Wa,-m405 -gdwarf-2       -c -o arch/powerpc/lib/copy_32.o arch/powerpc/lib/copy_32.S

deps_arch/powerpc/lib/copy_32.o := \
  arch/powerpc/lib/copy_32.S \
  /tools/dev/repos/linux-2.6-xlnx/arch/powerpc/include/asm/processor.h \
    $(wildcard include/config/vsx.h) \
    $(wildcard include/config/ppc32.h) \
    $(wildcard include/config/ppc/prep.h) \
    $(wildcard include/config/task/size.h) \
    $(wildcard include/config/kernel/start.h) \
    $(wildcard include/config/ppc64.h) \
    $(wildcard include/config/4xx.h) \
    $(wildcard include/config/booke.h) \
    $(wildcard include/config/altivec.h) \
    $(wildcard include/config/spe.h) \
  /tools/dev/repos/linux-2.6-xlnx/arch/powerpc/include/asm/reg.h \
    $(wildcard include/config/40x.h) \
    $(wildcard include/config/fsl/emb/perfmon.h) \
    $(wildcard include/config/8xx.h) \
    $(wildcard include/config/ppc/book3s/64.h) \
    $(wildcard include/config/ppc/book3s/32.h) \
    $(wildcard include/config/ppc/book3e/64.h) \
    $(wildcard include/config/e200.h) \
    $(wildcard include/config/ppc/cell.h) \
  include/linux/stringify.h \
  /tools/dev/repos/linux-2.6-xlnx/arch/powerpc/include/asm/cputable.h \
    $(wildcard include/config/smp.h) \
    $(wildcard include/config/mpc10x/bridge.h) \
    $(wildcard include/config/ppc/83xx.h) \
    $(wildcard include/config/8260.h) \
    $(wildcard include/config/ppc/mpc52xx.h) \
    $(wildcard include/config/bdi/switch.h) \
    $(wildcard include/config/power3.h) \
    $(wildcard include/config/power4.h) \
    $(wildcard include/config/44x.h) \
    $(wildcard include/config/e500.h) \
  /tools/dev/repos/linux-2.6-xlnx/arch/powerpc/include/asm/asm-compat.h \
    $(wildcard include/config/power4/only.h) \
    $(wildcard include/config/ibm405/err77.h) \
  /tools/dev/repos/linux-2.6-xlnx/arch/powerpc/include/asm/types.h \
    $(wildcard include/config/phys/64bit.h) \
  include/asm-generic/int-ll64.h \
  /tools/dev/repos/linux-2.6-xlnx/arch/powerpc/include/asm/bitsperlong.h \
  include/asm-generic/bitsperlong.h \
    $(wildcard include/config/64bit.h) \
  /tools/dev/repos/linux-2.6-xlnx/arch/powerpc/include/asm/feature-fixups.h \
  /tools/dev/repos/linux-2.6-xlnx/arch/powerpc/include/asm/reg_booke.h \
    $(wildcard include/config/403gcx.h) \
  /tools/dev/repos/linux-2.6-xlnx/arch/powerpc/include/asm/cache.h \
    $(wildcard include/config/ppc/e500mc.h) \
  /tools/dev/repos/linux-2.6-xlnx/arch/powerpc/include/asm/errno.h \
  include/asm-generic/errno.h \
  include/asm-generic/errno-base.h \
  /tools/dev/repos/linux-2.6-xlnx/arch/powerpc/include/asm/ppc_asm.h \
    $(wildcard include/config/virt/cpu/accounting.h) \
    $(wildcard include/config/ppc601/sync/fix.h) \
    $(wildcard include/config/ibm440ep/err42.h) \
  include/linux/init.h \
    $(wildcard include/config/modules.h) \
    $(wildcard include/config/hotplug.h) \
  include/linux/compiler.h \
    $(wildcard include/config/trace/branch/profiling.h) \
    $(wildcard include/config/profile/all/branches.h) \
    $(wildcard include/config/enable/must/check.h) \
    $(wildcard include/config/enable/warn/deprecated.h) \
  /tools/dev/repos/linux-2.6-xlnx/arch/powerpc/include/asm/ppc-opcode.h \

arch/powerpc/lib/copy_32.o: $(deps_arch/powerpc/lib/copy_32.o)

$(deps_arch/powerpc/lib/copy_32.o):
