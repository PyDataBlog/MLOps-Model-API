#CFLAGS = -g -DNO_ITERATOR
CFLAGS = -g
exe3.21:exe3.21.cpp
	g++ -Wall -std=c++11 $(CFLAGS) -o exe3.21 exe3.21.cpp
clean:	
	rm exe3.21
