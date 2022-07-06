SRCS = $(wildcard *.c)
PRGS = HelloWorld

CFLAGS = -Wall

all:	$(SRCS)
	$(CC) -o $(PRGS) $(CFLAGS) $<

clean:
	rm -f $(PRGS)
