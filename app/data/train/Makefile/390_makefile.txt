export R_LIBS_USER="~/R/library"

thesis: data/spud.sqlite
	mkdir -p ~/R/library
	Rscript dependencies.R
	Rscript -e "library(knitr); knit(\"thesis.Rnw\")"
	# Do the LaTeX, bibtex dance..
	pdflatex thesis.tex
	bibtex thesis
	bibtex bu1
	pdflatex thesis.tex
	pdflatex thesis.tex
	
data/spud.sqlite:
	wget -P ./data/ -nc http://www.dcs.gla.ac.uk/~daniel/spud/spud.zip
	unzip ./data/spud.zip -d data/
	# Remove zip once extracted OK
	if [ -e data/spud.sqlite ]; then rm data/spud.zip; fi;

compress:
	gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/prepress -dNOPAUSE -dQUIET -dBATCH -sOutputFile=thesis-min.pdf thesis.pdf

clean:
	rm -f *.pdf *.toc *.log *.cb *.cb2 *.aux *.lot *.tex *.blg *.bbl
	
deep-clean: clean
	# Warning, this deletes the non-reproducible cached figures too
	rm -rf cache
	rm -rf figure
