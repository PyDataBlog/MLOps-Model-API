CC=gcc

CFLAGS=-Wall -O2 -std=c99

all: fibonacci

fibonacci: fibonacci.c
	$(CC) $(CFLAGS) fibonacci.c -o $@

clean:
	rm -f fibonacci
