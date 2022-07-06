include ../Makefile.variable

all: interface_util.o functiontable.o

interface_util.o:
	$(CC) $(CFLAGS) $(INC) -c -o interface_util.o interface_util.c

functiontable.o: 
	$(CC) $(CFLAGS) $(INC) -c -o functiontable.o functiontable.c

copy:
	cp $(PLAYER)/libplayerinterface/interface_util.c ./
	cp $(PLAYER)/libplayerinterface/interface_util.h ./
	cp $(PLAYER)/libplayerinterface/functiontable.c ./
	cp $(PLAYER)/libplayerinterface/functiontable.h ./

clean:
	rm -f *.o

nuke:
	rm -f *.c
	rm -f *.h
