#This is C++ hello world Makefile

OBJS=hello

CC = g++
CPP_SOURCE = $(wildcard *.cpp)
OBJ_SOURCE = $(patsubst %.cpp,%.o, $(CPP_SOURCE))

all:$(OBJ_SOURCE)
	$(CC) -o $(OBJS) $(OBJ_SOURCE)

%.o:%.cpp
	$(CC) -c $^ -o $@

.PHONY:clean
clean:
	rm -rf *.o $(OBJS)
