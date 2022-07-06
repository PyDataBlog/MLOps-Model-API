all:jupiter
jupiter:main.c
	gcc -g -O0 -o $@ $^
clean:
	-rm jupiter
.PHONY:all clean
