CC   = gcc
CFLAGS += -Wall -Wextra

ifeq ($(DEBUG),1)
CFLAGS += -g3
endif

SRCS = $(wildcard *.c)
PROGS = $(patsubst %.c,%,$(SRCS))

all: $(PROGS)

%: %.c
	$(CC) $(CFLAGS) -o $@ $<
clean:
	rm -rf *[!.c][!Makefile]
