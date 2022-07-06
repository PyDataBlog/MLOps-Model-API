BINDIR = /usr/local/bin

all : mkpatch

install : $(BINDIR)/mkpatch

$(BINDIR)/mkpatch : mkpatch
	install -m 555 $< $@

