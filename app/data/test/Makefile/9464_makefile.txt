LATEX=lualatex
RM=rm -f

all: resume

resume: resume.tex
	$(LATEX) resume.tex

clean:
	$(RM) *.aux *.log *.out

dist-clean: clean
	$(RM) resume.pdf
