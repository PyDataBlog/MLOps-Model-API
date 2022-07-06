cmd_.tmp_kallsyms1.o := arm-eabi-gcc -Wp,-MD,./..tmp_kallsyms1.o.d -D__ASSEMBLY__ -mabi=aapcs-linux -mno-thumb-interwork  -D__LINUX_ARM_ARCH__=7 -march=armv7-a  -include asm/unified.h -msoft-float    -nostdinc -isystem /home/tronit/opt/Android_Toolchains-arm-eabi-4.4.3/arm-eabi-4.4.3/bin/../lib/gcc/arm-eabi/4.4.3/include -I/home/tronit/KERNEL_BLACKDOME_4.2-4.1/android_kernel_htc_qsd8k-jellybean/arch/arm/include -Iinclude  -include include/generated/autoconf.h -D__KERNEL__ -mlittle-endian -Iarch/arm/mach-msm/include    -c -o .tmp_kallsyms1.o .tmp_kallsyms1.S

source_.tmp_kallsyms1.o := .tmp_kallsyms1.S

deps_.tmp_kallsyms1.o := \
  /home/tronit/KERNEL_BLACKDOME_4.2-4.1/android_kernel_htc_qsd8k-jellybean/arch/arm/include/asm/unified.h \
    $(wildcard include/config/arm/asm/unified.h) \
    $(wildcard include/config/thumb2/kernel.h) \
  /home/tronit/KERNEL_BLACKDOME_4.2-4.1/android_kernel_htc_qsd8k-jellybean/arch/arm/include/asm/types.h \
  include/asm-generic/int-ll64.h \
  /home/tronit/KERNEL_BLACKDOME_4.2-4.1/android_kernel_htc_qsd8k-jellybean/arch/arm/include/asm/bitsperlong.h \
  include/asm-generic/bitsperlong.h \
    $(wildcard include/config/64bit.h) \

.tmp_kallsyms1.o: $(deps_.tmp_kallsyms1.o)

$(deps_.tmp_kallsyms1.o):
