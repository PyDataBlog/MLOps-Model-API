cmd_arch/arm/lib/io-readsl.o := arm-poky-linux-gnueabi-gcc -Wp,-MD,arch/arm/lib/.io-readsl.o.d  -nostdinc -isystem /opt/poky/1.7/sysroots/i686-pokysdk-linux/usr/lib/arm-poky-linux-gnueabi/gcc/arm-poky-linux-gnueabi/4.9.1/include -I/second_extend/yocto_file/linux_yocto/arch/arm/include -Iarch/arm/include/generated  -Iinclude -I/second_extend/yocto_file/linux_yocto/arch/arm/include/uapi -Iarch/arm/include/generated/uapi -I/second_extend/yocto_file/linux_yocto/include/uapi -Iinclude/generated/uapi -include /second_extend/yocto_file/linux_yocto/include/linux/kconfig.h -D__KERNEL__ -mlittle-endian  -D__ASSEMBLY__ -mabi=aapcs-linux -mno-thumb-interwork -mfpu=vfp -funwind-tables -marm -D__LINUX_ARM_ARCH__=7 -march=armv7-a  -include asm/unified.h -msoft-float         -c -o arch/arm/lib/io-readsl.o arch/arm/lib/io-readsl.S

source_arch/arm/lib/io-readsl.o := arch/arm/lib/io-readsl.S

deps_arch/arm/lib/io-readsl.o := \
  /second_extend/yocto_file/linux_yocto/arch/arm/include/asm/unified.h \
    $(wildcard include/config/arm/asm/unified.h) \
    $(wildcard include/config/thumb2/kernel.h) \
  include/linux/linkage.h \
  include/linux/compiler.h \
    $(wildcard include/config/sparse/rcu/pointer.h) \
    $(wildcard include/config/trace/branch/profiling.h) \
    $(wildcard include/config/profile/all/branches.h) \
    $(wildcard include/config/enable/must/check.h) \
    $(wildcard include/config/enable/warn/deprecated.h) \
    $(wildcard include/config/kprobes.h) \
  include/linux/stringify.h \
  include/linux/export.h \
    $(wildcard include/config/have/underscore/symbol/prefix.h) \
    $(wildcard include/config/modules.h) \
    $(wildcard include/config/modversions.h) \
    $(wildcard include/config/unused/symbols.h) \
  /second_extend/yocto_file/linux_yocto/arch/arm/include/asm/linkage.h \
  /second_extend/yocto_file/linux_yocto/arch/arm/include/asm/assembler.h \
    $(wildcard include/config/cpu/endian/be8.h) \
    $(wildcard include/config/cpu/feroceon.h) \
    $(wildcard include/config/trace/irqflags.h) \
    $(wildcard include/config/cpu/v7m.h) \
    $(wildcard include/config/smp.h) \
    $(wildcard include/config/cpu/use/domains.h) \
  /second_extend/yocto_file/linux_yocto/arch/arm/include/asm/ptrace.h \
    $(wildcard include/config/arm/thumb.h) \
  /second_extend/yocto_file/linux_yocto/arch/arm/include/uapi/asm/ptrace.h \
  /second_extend/yocto_file/linux_yocto/arch/arm/include/asm/hwcap.h \
  /second_extend/yocto_file/linux_yocto/arch/arm/include/uapi/asm/hwcap.h \
  /second_extend/yocto_file/linux_yocto/arch/arm/include/asm/domain.h \
    $(wildcard include/config/io/36.h) \
  /second_extend/yocto_file/linux_yocto/arch/arm/include/asm/opcodes-virt.h \
  /second_extend/yocto_file/linux_yocto/arch/arm/include/asm/opcodes.h \
    $(wildcard include/config/cpu/endian/be32.h) \

arch/arm/lib/io-readsl.o: $(deps_arch/arm/lib/io-readsl.o)

$(deps_arch/arm/lib/io-readsl.o):
