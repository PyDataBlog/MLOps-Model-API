SHELL = /bin/sh
INSTALL = /usr/bin/install
INSTALL_PROGRAM = $(INSTALL) -m 0755
INSTALL_DATA = $(INSTALL) -m 0644
INSTALL_DIR = $(INSTALL) -d
LN = /usr/bin/ln
LN_S = $(LN) -s

bindir = /usr/local/bin
homebindir = $(HOME)/bin

bin_PROGRAMS = live-wallpaper

INSTALLFILES = $(bin_PROGRAMS:%=install-%)
UNINSTALLFILES = $(bin_PROGRAMS:%=uninstall-%)
HOMINGFILES = $(bin_PROGRAMS:%=install_home-%)
EVICTFILES = $(bin_PROGRAMS:%=uninstall_home-%)

all: build
build: $(bin_PROGRAMS)

install: $(INSTALLFILES)
$(INSTALLFILES): build
	$(INSTALL_PROGRAM) $(@:install-%=%) $(DESTDIR)$(bindir)/

install_home: $(HOMINGFILES)
$(HOMINGFILES): build
	$(INSTALL_DIR) $(homebindir)/
	$(INSTALL_PROGRAM) $(@:install_home-%=%) $(homebindir)/

uninstall: $(UNINSTALLFILES)
$(UNINSTALLFILES):
	$(RM) $(DESTDIR)$(bindir)/$(@:uninstall-%=%)

uninstall_home: $(EVICTFILES)
$(EVICTFILES):
	$(RM) $(homebindir)/$(@:uninstall_home-%=%)

test: build

clean:

.PHONY: $(INSTALLFILES) $(HOMINGFILES) $(UNINSTALLFILES) $(EVICTFILES)
.PHONY: install install_home uninstall uninstall_home 
.PHONY: all build clean test