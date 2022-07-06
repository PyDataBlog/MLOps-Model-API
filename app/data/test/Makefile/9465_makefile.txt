# CFLAGS = -pedantic-errors -Wall -Wextra -I. -lm -std=c89
CFLAGS = -I. -lm -std=gnu11 -Wno-variadic-macros
OBJECT_PATH = obj/

SOURCES = main.c bitop.c

.PHONY: debug main all clean

debug:
	gcc $(CFLAGS) -g -o $(OBJECT_PATH)debug.o $(SOURCES) -DDEBUG

main:
	gcc $(CFLAGS) -o $(OBJECT_PATH)main.o $(SOURCES)

all:	debug main

clean:
	rm $(OBJECT_PATH)main.o
	rm $(OBJECT_PATH)debug.o
