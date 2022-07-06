TEX = main
LATEX = xelatex
DVIPDF = dvipdfm
SPELL = aspell
BIBTEX = bibtex
SOURCES = $(wildcard *.tex) $(wildcard *.bib)

default: pdf

pdf: $(SOURCES)
	$(LATEX) $(TEX); $(BIBTEX) $(TEX); $(LATEX) $(TEX); $(LATEX) $(TEX)

single-pass: $(SOURCES)
	$(LATEX) $(TEX)

view:
	$(LATEX) $(TEX)
	open $(TEX).pdf

check:
	@echo "Passing the check will cause make to report Error 1."
	$(LATEX) $(TEX)  | grep -i undefined

clean:
	$(RM) -rf *.aux *.blg *.dvi *.log *.toc *.lof *.lot *.cb *.bbl \
		$(TEX).ps $(TEX).pdf *~ *.out auto/ *.ent doc.tex

.PHONY: pdf single-pass view check clean
