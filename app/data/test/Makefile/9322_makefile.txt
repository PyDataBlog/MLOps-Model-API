HOSTCC = gcc

WARNMSG := -Wall -Wmissing-prototypes -Wstrict-prototypes
basic:
	echo "HOSTCC fixdep"
	@$(HOSTCC) $(WARNMSG)  -O2 -fomit-frame-pointer -o fixdep fixdep.c;

script:
	echo "HOSTCC fixdep"
	@$(HOSTCC) $(WARNMSG) -O2 -fomit-frame-pointer -DCURSES_LOC="<ncurses.h>" -DLOCALE -c -o conf.o conf.c; 

lxdialog: FALSE
	@$(HOSTCC) $(WARNMSG) -O2 -fomit-frame-pointer -DCURSES_LOC="<ncurses.h>" -DLOCALE -c -o lxdialog/checklist.o lxdialog/checklist.c; 
	@$(HOSTCC) $(WARNMSG) -O2 -fomit-frame-pointer -DCURSES_LOC="<ncurses.h>" -DLOCALE -c -o lxdialog/inputbox.o lxdialog/inputbox.c; 
	@$(HOSTCC) $(WARNMSG) -O2 -fomit-frame-pointer -DCURSES_LOC="<ncurses.h>" -DLOCALE -c -o lxdialog/menubox.o lxdialog/menubox.c; 
	@$(HOSTCC) $(WARNMSG) -O2 -fomit-frame-pointer -DCURSES_LOC="<ncurses.h>" -DLOCALE -c -o lxdialog/textbox.o lxdialog/textbox.c; 
	@$(HOSTCC) $(WARNMSG) -O2 -fomit-frame-pointer -DCURSES_LOC="<ncurses.h>" -DLOCALE -c -o lxdialog/util.o lxdialog/util.c; \
#	@./fixdep lxdialog/.util.o.d lxdialog/util.o 'gcc -Wp,-MD,lxdialog/.util.o.d -Wall -Wmissing-prototypes -Wstrict-prototypes -O2 -fomit-frame-pointer -DCURSES_LOC="<ncurses.h>" -DLOCALE   -c -o lxdialog/util.o lxdialog/util.c' > lxdialog/.util.o.tmp; 
	@$(HOSTCC) $(WARNMSG) -O2 -fomit-frame-pointer -DCURSES_LOC="<ncurses.h>" -DLOCALE -c -o lxdialog/yesno.o lxdialog/yesno.c; 
zconf.tab.o: zconf.lex.c zconf.hash.c
	@$(HOSTCC) -Wp,-MD,.zconf.tab.o.d $(WARNMSG) -O2 -fomit-frame-pointer -DCURSES_LOC="<ncurses.h>" -DLOCALE -I./ -c -o zconf.tab.o zconf.tab.c; \
#	@./fixdep .zconf.tab.o.d zconf.tab.o 'gcc -Wp,-MD,.zconf.tab.o.d -Wall -Wmissing-prototypes -Wstrict-prototypes -O2 -fomit-frame-pointer   -DCURSES_LOC="<ncurses.h>" -DLOCALE  -Ikconfig -c -o zconf.tab.o zconf.tab.c' > .zconf.tab.o.tmp;

oskConf: basic script lxdialog zconf.tab.o
	@$(HOSTCC) $(WARNMSG) -O2 -fomit-frame-pointer -DCURSES_LOC="<ncurses.h>" -DLOCALE -c -o mconf.o mconf.c; 
	@gcc  -o oskConf mconf.o zconf.tab.o lxdialog/checklist.o lxdialog/util.o lxdialog/inputbox.o lxdialog/textbox.o lxdialog/yesno.o lxdialog/menubox.o  -lncurses; 
all: oskConf
	cp oskConf TestCase

clean:
	@find . -name "*.o" -print -exec rm -rf {} \;
	@find . -name "*.o.tmp" -print -exec rm -rf {} \;
	@find . -name "*.o.d" -print -exec rm -rf {} \;
	@rm -f oskConf log.txt fixdep;
	

FALSE: