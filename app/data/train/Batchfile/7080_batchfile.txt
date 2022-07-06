cmd_arch/powerpc/boot/cuboot-hotfoot.o := ppc_4xx-gcc -m32 -Wp,-MD,arch/powerpc/boot/.cuboot-hotfoot.o.d -Wall -Wundef -Wstrict-prototypes -Wno-trigraphs -fno-strict-aliasing -Os -msoft-float -pipe -fomit-frame-pointer -fno-builtin -fPIC -nostdinc -isystem /tools/dev/eldk/usr/bin/../lib/gcc/powerpc-linux/4.2.2/include -g -fno-stack-protector -Iarch/powerpc/boot -I/tools/dev/repos/linux-2.6-xlnx/arch/powerpc/boot -mcpu=405 -c -o arch/powerpc/boot/cuboot-hotfoot.o arch/powerpc/boot/cuboot-hotfoot.c

deps_arch/powerpc/boot/cuboot-hotfoot.o := \
  arch/powerpc/boot/cuboot-hotfoot.c \
  arch/powerpc/boot/ops.h \
  /tools/dev/eldk/usr/bin/../lib/gcc/powerpc-linux/4.2.2/include/stddef.h \
  arch/powerpc/boot/types.h \
  arch/powerpc/boot/string.h \
  arch/powerpc/boot/stdio.h \
  /tools/dev/eldk/usr/bin/../lib/gcc/powerpc-linux/4.2.2/include/stdarg.h \
  arch/powerpc/boot/reg.h \
  arch/powerpc/boot/dcr.h \
    $(wildcard include/config/bank/enable.h) \
    $(wildcard include/config/size/mask.h) \
    $(wildcard include/config/bank/size.h) \
    $(wildcard include/config/addr.h) \
    $(wildcard include/config/data.h) \
  arch/powerpc/boot/4xx.h \
  arch/powerpc/boot/cuboot.h \
  arch/powerpc/boot/ppcboot-hotfoot.h \

arch/powerpc/boot/cuboot-hotfoot.o: $(deps_arch/powerpc/boot/cuboot-hotfoot.o)

$(deps_arch/powerpc/boot/cuboot-hotfoot.o):
