MAKEFLAGS += -j1


PREFIX=$(abspath ./build)

all: 
	@echo "You are ready to build VLC and its contribs"

include tools.mak
