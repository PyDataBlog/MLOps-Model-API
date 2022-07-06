
all: hello.out

hello.out: hello.asm
	nasm -f elf hello.asm
	ld -m elf_i386 -s -o build/hello hello.o
	build/hello > output/hello.out

recompile:
	rm -f *.out
	make all

clean:
	rm -f *.o
	rm -f *~
	rm -f build/*
	rm -f output/*
	rm -f hello

initial:
	rm -f *.out
	make clean

raw: hello.bin

hello.bin: hello.asm
#make a raw image
	nasm -f bin -o hello.bin hello.asm
#convert to floppy disk image
	dd status=noxfer conv=notrunc if=hello.bin of=hello.flp
#or to an iso, to load using a cd/penD
#	mkisofs -o hello.iso -b hello.flp cdiso/

simulate: hello.bin
	qemu -fda hello.flp
