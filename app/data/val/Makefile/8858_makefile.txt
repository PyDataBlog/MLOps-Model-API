# Useful for `make dist` only!

.PHONY: all

all:
	for template in $(wildcard *.html); do \
		sed 's/\/~vishwin\/eon\/2014/\/eon\/$(DESTDIRSTEM)/g' $$template > $(TEMPLATEDIR)/$$template; \
	done
	sed -i 's/\/bower_components\/modernizr/\/js/g' $(TEMPLATEDIR)/meta.html
	sed -i -f footer.sed $(TEMPLATEDIR)/footer.html
