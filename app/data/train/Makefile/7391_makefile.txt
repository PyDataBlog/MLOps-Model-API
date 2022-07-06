# Generate HTML pages with a menu

SRCS += index.md
SRCS += getting_started.md
SRCS += screenshots.md
SRCS += downloads.md
SRCS += documentation.md
SRCS += developers.md

BUILD_DIR=.

HTMLS = $(SRCS:%.md=%.html)
.SECONDARY:
TITLES = $(SRCS:%.md=titles.d/%.title)

DEPENDS = header.html footer.html ../gen_menu.sh

all: $(HTMLS)

titles.d:
	@mkdir titles.d

titles.d/%.title: %.md titles.d
	@grep -m 1 "^#" $< > $@.tmp
	@cmp $@ $@.tmp || cp $@.tmp $@
	@rm -f $@.tmp

%.html: %.md $(DEPENDS) $(TITLES)
	sh ../gen_menu.sh --header header.html --footer footer.html --page $< -- $(SRCS) > $@
	
clean:
	rm -f $(HTMLS)
	rm -rf titles.d

print:
	echo SRCS=$(SRCS)
