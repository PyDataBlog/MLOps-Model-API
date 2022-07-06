# Var
RULE = Makefile.rule
SRC_BASE = ../..
SRC = $(wildcard *.c)
SRC_OBJ = $(SRC:.c=.o)
SRC_LIB = libtools.a
CPPFLAGS = -I$(SRC_BASE)/inc

include $(SRC_BASE)/$(RULE)

# target
all: lib
