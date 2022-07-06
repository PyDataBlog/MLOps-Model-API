#########################
# Makefile for Orange'S #
#########################

# Entry point of Orange'S
# It must have the same value with 'KernelEntryPointPhyAddr' in load.inc!
ENTRYPOINT	= 0x30400
include ../../commk/Makefile
# Offset of entry point in kernel file
# It depends on ENTRYPOINT
ENTRYOFFSET	=   0x400

# Programs, flags, etc.
ASM_BOOT_SEARCH_PATH 	= -I boot/include/
ASM_KERNEL_SEARCH_PATH	= -I include/
GCC_COMP_SEARCH_PATH	= -I include/ $(GCC_NO_STDINC) 

# compile parameters
ASMBFLAGS	= $(ASM_BOOT_SEARCH_PATH)
ASMKFLAGS	= $(ASM_KERNEL_SEARCH_PATH) $(ELFFLAGS)
CFLAGS		= $(GCC_COMP_SEARCH_PATH) $(GCC_COMPILE) $(GCC_NO_BUILDIN) $(GCC_ARC_I386) $(GCC_NO_STACK_PROTECT) 
LDFLAGS		= $(STRIPALL) -Ttext $(ENTRYPOINT) $(ARC_I386) 
DASMFLAGS	= -u -o $(ENTRYPOINT) -e $(ENTRYOFFSET)

# This Program
ORANGESBOOT	= boot/boot.bin boot/loader.bin
ORANGESKERNEL	= kernel.bin
OBJS		= kernel/kernel.o kernel/start.o kernel/main.o kernel/i8259.o kernel/global.o kernel/protect.o lib/kliba.o lib/klib.o lib/string.o
DASMOUTPUT	= kernel.bin.asm
IMG			= a.img

# All Phony Targets
.PHONY : everything final image clean realclean disasm all buildimg stage1 stage2 stage3 stage4 one two three four five lastone

# Default starting position
nop :
	@echo "make stage1|stage2|stage3"

stage1 : one image

stage2 : two image

stage3 : three image

one :
	cp kernel/kernel1.asm kernel/kernel.asm

two :
	cp kernel/kernel2.asm kernel/kernel.asm

three :
	cp kernel/kernel3.asm kernel/kernel.asm

everything : $(ORANGESBOOT) $(ORANGESKERNEL)

all : realclean everything

image : realclean everything clean buildimg

clean :
	make clean -C kernel;
	make clean -C lib;
	make clean -C boot;

realclean : clean	
	rm -f $(IMG) $(ORANGESKERNEL);

disasm :
	$(DASM) $(DASMFLAGS) $(ORANGESKERNEL) > $(DASMOUTPUT)

# We assume that "a.img" exists in current folder
buildimg : $(IMG)
	dd if=boot/boot.bin of=$(IMG) bs=512 count=1 conv=notrunc
	sudo mount -o loop $(IMG) /mnt/floppy/
	sudo cp -fv boot/loader.bin /mnt/floppy/
	sudo cp -fv $(ORANGESKERNEL) /mnt/floppy
	sudo umount /mnt/floppy

boot/boot.bin : 
	make boot.bin -C boot;

boot/loader.bin :
	make loader.bin -C boot;

$(ORANGESKERNEL) : $(OBJS)
	$(LD) $(LDFLAGS) -o $(ORANGESKERNEL) $(OBJS)

kernel/kernel.o : 
	make kernel.o -C kernel;

kernel/start.o:
	make start.o -C kernel;

kernel/main.o:
	make main.o -C kernel;

kernel/i8259.o:
	make i8259.o -C kernel;

kernel/global.o: 
	make global.o -C kernel;

kernel/protect.o: 
	make protect.o -C kernel;

lib/klib.o:
	make klib.o -C lib;

lib/kliba.o : 
	make kliba.o -C lib;

lib/string.o :
	make string.o -C lib;
	
$(IMG):
	qemu-img create -f raw $(IMG) 1440K;
