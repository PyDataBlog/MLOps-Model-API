
%.pdf : %.fig
	fig2dev -L pdf $< > $@

.PHONY : all

all : how-to-program.pdf

how-to-program.pdf : how-to-program.tex logo.pdf
	pdflatex $<
	pdflatex $<

clean :
	@rm -fv *.aux
	@rm -fv *.log
	@rm -fv *.out
	@rm -fv logo.pdf
	@rm -fv how-to-program.pdf
