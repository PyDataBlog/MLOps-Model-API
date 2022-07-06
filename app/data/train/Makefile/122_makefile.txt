MAIN=		sosplice-slides

LATEX?=		latex </dev/null
XDVI?=		xdvi -paper a4
DVIPS?=		dvips
GV?=		gv
PSPDF?=		ps2pdf
DVIPDF?=	dvipdf
XPDF?=		xpdf
ISPELL?=	ispell -t

.PHONY: all clean dvi ps pdf xdvi gv xpdf

all: dvi ps pdf
dvi: ${MAIN}.dvi
ps: ${MAIN}.ps
pdf: ${MAIN}.pdf

${MAIN}.dvi: *.tex

xdvi: ${MAIN}.dvi
	${XDVI} ${MAIN} &
gv: ${MAIN}.ps
	${GV} ${MAIN} &
xpdf: ${MAIN}.pdf
	${XPDF} ${MAIN}.pdf &
ispell: ${MAIN}.tex
	${ISPELL} ${MAIN}.tex

clean:
	rm -f *.dvi *.log *.toc *.lof *.lot *.aux *.idx *.ind *.ilg *.ps *.pdf

.SUFFIXES: .tex .dvi .ps .pdf

.tex.dvi:
	${LATEX} $*
.dvi.ps:
	${DVIPS} -o $@ $*
.ps.pdf:
	${PSPDF} $< $@
.dvi.pdf:
	${DVIPDF} $< $@
