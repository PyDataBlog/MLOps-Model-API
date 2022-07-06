cmd_drivers/video/logo/logo_linux_mono.o := ppc_4xx-gcc -m32 -Wp,-MD,drivers/video/logo/.logo_linux_mono.o.d  -nostdinc -isystem /tools/dev/eldk/usr/bin/../lib/gcc/powerpc-linux/4.2.2/include -I/tools/dev/repos/linux-2.6-xlnx/arch/powerpc/include -Iinclude  -include include/generated/autoconf.h -D__KERNEL__ -Iarch/powerpc -Wall -Wundef -Wstrict-prototypes -Wno-trigraphs -fno-strict-aliasing -fno-common -Werror-implicit-function-declaration -Wno-format-security -fno-delete-null-pointer-checks -O2 -msoft-float -pipe -Iarch/powerpc -ffixed-r2 -mmultiple -mno-altivec -mno-spe -mspe=no -funit-at-a-time -mno-string -Wa,-m405 -fno-stack-protector -fomit-frame-pointer -g -Wdeclaration-after-statement -Wno-pointer-sign -fno-strict-overflow   -D"KBUILD_STR(s)=\#s" -D"KBUILD_BASENAME=KBUILD_STR(logo_linux_mono)"  -D"KBUILD_MODNAME=KBUILD_STR(logo_linux_mono)"  -c -o drivers/video/logo/.tmp_logo_linux_mono.o drivers/video/logo/logo_linux_mono.c

deps_drivers/video/logo/logo_linux_mono.o := \
  drivers/video/logo/logo_linux_mono.c \
  include/linux/linux_logo.h \
    $(wildcard include/config/fb/logo/extra.h) \
  include/linux/init.h \
    $(wildcard include/config/modules.h) \
    $(wildcard include/config/hotplug.h) \
  include/linux/compiler.h \
    $(wildcard include/config/trace/branch/profiling.h) \
    $(wildcard include/config/profile/all/branches.h) \
    $(wildcard include/config/enable/must/check.h) \
    $(wildcard include/config/enable/warn/deprecated.h) \
  include/linux/compiler-gcc.h \
    $(wildcard include/config/arch/supports/optimized/inlining.h) \
    $(wildcard include/config/optimize/inlining.h) \
  include/linux/compiler-gcc4.h \

drivers/video/logo/logo_linux_mono.o: $(deps_drivers/video/logo/logo_linux_mono.o)

$(deps_drivers/video/logo/logo_linux_mono.o):
