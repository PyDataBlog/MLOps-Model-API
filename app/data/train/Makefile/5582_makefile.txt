all:iaalm.sty

iaalm.sty:iaalm.ins iaalm.dtx
	xelatex $<

install:all
	mkdir -p /usr/share/texmf/tex/xelatex/iaalm/
	cp iaalm.sty /usr/share/texmf/tex/xelatex/iaalm/
	texhash

