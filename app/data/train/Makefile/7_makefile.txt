
all:
	g++ lpf.cpp -o lpf.o
.PHONY:
	clean
clean:
	rm *.o
