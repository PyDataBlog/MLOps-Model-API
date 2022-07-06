.PHONY: all clean

all: 
	pdflatex technologierecherche.tex
	pdflatex technologierecherche.tex
	pdflatex technologierecherche.tex

clean:
	rm -f *.aux
	rm -f *.bbl
	rm -f *.blg
	rm -f *.log
	rm -f *.out
	rm -f *.toc
	rm -f *.lot
	rm -f *.lof

