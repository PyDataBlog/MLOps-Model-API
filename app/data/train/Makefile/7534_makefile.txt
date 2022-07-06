all:
	clang main.m -I /usr/include/GNUstep/ -D_NATIVE_OBJC_EXCEPTIONS -lgnustep-base -fconstant-string-class=NSConstantString -g -o t
	clang erro.m -I /usr/include/GNUstep/ -D_NATIVE_OBJC_EXCEPTIONS -lgnustep-base -fconstant-string-class=NSConstantString -g -o s
clean:
	rm -f s t *.o
