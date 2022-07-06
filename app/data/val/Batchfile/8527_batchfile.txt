cmd_arch/x86/kernel/acpi/realmode/wakeup.o := gcc -Wp,-MD,arch/x86/kernel/acpi/realmode/.wakeup.o.d  -nostdinc -isystem /usr/lib/gcc/x86_64-linux-gnu/4.8/include -D__KERNEL__ -Iinclude  -include include/linux/autoconf.h -Iinclude  -include include/linux/autoconf.h -g -Os -D_SETUP -D_WAKEUP -D__KERNEL__ -I/home/nhong/workspace/kernel-tut/arch/x86/kernel/acpi/realmode/../../../boot  -Wall -Wstrict-prototypes -march=i386 -mregparm=3 -include /home/nhong/workspace/kernel-tut/arch/x86/kernel/acpi/realmode/../../../boot/code16gcc.h -fno-strict-aliasing -fomit-frame-pointer  -ffreestanding  -fno-toplevel-reorder  -fno-stack-protector  -mpreferred-stack-boundary=2  -m32 -D__ASSEMBLY__     -c -o arch/x86/kernel/acpi/realmode/wakeup.o arch/x86/kernel/acpi/realmode/wakeup.S

deps_arch/x86/kernel/acpi/realmode/wakeup.o := \
  arch/x86/kernel/acpi/realmode/wakeup.S \
    $(wildcard include/config/64bit.h) \
  /home/nhong/workspace/kernel-tut/arch/x86/kernel/acpi/realmode/../../../boot/code16gcc.h \
  include/asm/segment.h \
    $(wildcard include/config/x86/32.h) \
    $(wildcard include/config/smp.h) \
    $(wildcard include/config/paravirt.h) \
  include/asm/msr-index.h \
  include/asm/page.h \
    $(wildcard include/config/x86/64.h) \
  include/linux/const.h \
  include/asm/page_32.h \
    $(wildcard include/config/highmem4g.h) \
    $(wildcard include/config/highmem64g.h) \
    $(wildcard include/config/page/offset.h) \
    $(wildcard include/config/x86/pae.h) \
    $(wildcard include/config/hugetlb/page.h) \
    $(wildcard include/config/flatmem.h) \
    $(wildcard include/config/x86/use/3dnow.h) \
    $(wildcard include/config/x86/3dnow.h) \
  include/asm-generic/memory_model.h \
    $(wildcard include/config/discontigmem.h) \
    $(wildcard include/config/sparsemem/vmemmap.h) \
    $(wildcard include/config/sparsemem.h) \
    $(wildcard include/config/out/of/line/pfn/to/page.h) \
  include/asm-generic/page.h \
  include/asm/pgtable.h \
  include/asm/pgtable_32.h \
    $(wildcard include/config/highmem.h) \
    $(wildcard include/config/highpte.h) \
  include/asm/processor-flags.h \

arch/x86/kernel/acpi/realmode/wakeup.o: $(deps_arch/x86/kernel/acpi/realmode/wakeup.o)

$(deps_arch/x86/kernel/acpi/realmode/wakeup.o):
