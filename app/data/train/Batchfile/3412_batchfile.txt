cmd_arch/powerpc/boot/crt0.o := ppc_4xx-gcc -m32 -Wp,-MD,arch/powerpc/boot/.crt0.o.d -D__ASSEMBLY__ -Wall -Wundef -Wstrict-prototypes -Wno-trigraphs -fno-strict-aliasing -Os -msoft-float -pipe -fomit-frame-pointer -fno-builtin -fPIC -nostdinc -isystem /tools/dev/eldk/usr/bin/../lib/gcc/powerpc-linux/4.2.2/include -traditional -nostdinc -c -o arch/powerpc/boot/crt0.o arch/powerpc/boot/crt0.S

deps_arch/powerpc/boot/crt0.o := \
  arch/powerpc/boot/crt0.S \
  arch/powerpc/boot/ppc_asm.h \

arch/powerpc/boot/crt0.o: $(deps_arch/powerpc/boot/crt0.o)

$(deps_arch/powerpc/boot/crt0.o):
