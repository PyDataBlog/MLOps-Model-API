all:

install: all
	install -d $(DESTDIR)/usr/share/pam-configs/
	install -o root -g root -m 644 ceibal.pam.config $(DESTDIR)/usr/share/pam-configs/ceibal
