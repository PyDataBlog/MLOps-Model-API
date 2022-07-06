#! /usr/bin/make -f
SHELL=/bin/sh

destdir?=/usr/local
prefix ?=${destdir}
bindir ?=${prefix}/bin
mandir ?=${prefix}/share/man/man1

# files that need mode 755
EXEC_FILES=git-ftp

# files that need mode 644
MAN_FILE=git-ftp.1

all:
	@echo "usage: make install     -> installs git-ftp only"
	@echo "       make install-man -> installs man pages only"
	@echo "       make install-all -> installs git-ftp and man pages"
	@echo "       make uninstall"
	@echo "       make uninstall-man"
	@echo "       make uninstall-all"
	@echo "       make clean"

install:
	install -d -m 0755 $(bindir)
	install -m 0755 $(EXEC_FILES) $(bindir)

install-man:
	mkdir -p $(mandir)
	cd man && \
	make man && \
	install -m 0644 $(MAN_FILE) $(mandir)
	mandb $(mandir) 2>/dev/null || exit 0

install-all: install install-man

uninstall:
	test -d $(bindir) && \
	cd $(bindir) && \
	rm -f $(EXEC_FILES)

uninstall-man:
	test -d $(mandir) && \
	cd $(mandir) && \
	rm -f $(MAN_FILE)
	mandb -f $(mandir)/$(MAN_FILE)
	rmdir --ignore-fail-on-non-empty $(mandir)

uninstall-all: uninstall uninstall-man

clean:
	cd share/man && make clean
