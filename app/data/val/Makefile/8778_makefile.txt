# Compiler
#CC = clang

# Compiler flags:
# 	-std=c11 : Use c11 (latest standard)
#	-g : Add debugging info to binary
#	-Weverything : EVERY warning
#	-Wpedantic : Issue warnings about straying from ISO C

remind: remind.o readline.o
	clang -o remind remind.o readline.o

remind.o: remind.c readline.h
	clang -std=c11 -g -Weverything -Wpedantic -c remind.c

read_line.o: readline.c readline.h
	clang -std=c11 -g -Weverything -Wpedantic -c readline.c

clean:
	rm *.o; rm remind	
