TARGET  := main
SRCS    := $(wildcard *.c)
OBJS    := ${SRCS:.c=.o} 

CCFLAGS = 
LDFLAGS = 
LIBS    = 

.PHONY: all clean distclean 
all: ${TARGET} 

${TARGET}: ${OBJS} 
	${CC} ${LDFLAGS} -o $@ $^ ${LIBS} 

clean:: 
	-rm -f *~ *.o ${TARGET} 

distclean:: clean
