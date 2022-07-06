# the compiler: gcc for C program, define as g++ for C++
CPP = g++

OBJS = weather.o

# compiler flags:
#  -g    adds debugging information to the executable file
#  -Wall turns on most, but not all, compiler warnings
CFLAGS  = -g -Wall


# the build target executable:
TARGET = weather

all: $(TARGET)

$(TARGET): $(OBJS)
	$(CPP) -g -lm -lc -lcurl -ljson -pthread -o $@ $^

%.o : %.cpp
	$(CPP) $(CFLAGS) -o $@ -c $<

clean:
	$(RM) $(TARGET) $(OBJS)
