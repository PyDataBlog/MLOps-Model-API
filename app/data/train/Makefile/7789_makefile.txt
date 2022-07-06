PY=python
PANDOC=pandoc

BASEDIR=$(CURDIR)/pdf
INPUTDIR=$(CURDIR)/_posts
OUTPUTDIR=$(BASEDIR)/
STYLEDIR=$(BASEDIR)/style

help:
	@echo ' 																	  '
	@echo 'Makefile for MIMIC guidelines                                          '
	@echo '                                                                       '
	@echo 'Usage:                                                                 '
	@echo '   make pdf                         generate a PDF file  			  '
	@echo '   make docx	                       generate a Docx file 			  '
	@echo '                                                                       '
	@echo ' 																	  '
	@echo ' 																	  '
	@echo 'get local templates with: pandoc -D latex/html/etc	  				  '
	@echo 'or generic ones from: https://github.com/jgm/pandoc-templates		  '

pdf:
	pandoc $(STYLEDIR)/frontpage.md \
	$(STYLEDIR)/table_of_contents.md \
	$(INPUTDIR)/2015-04-22-introduction.md \
	$(INPUTDIR)/2015-04-24-whats-new.md \
	$(INPUTDIR)/2015-04-23-contributing.md \
	$(INPUTDIR)/2015-05-18-first-steps.md \
	$(INPUTDIR)/2015-05-18-installation.md \
	$(INPUTDIR)/2015-05-18-overview.md \
	$(INPUTDIR)/2015-04-22-additives.md \
	$(INPUTDIR)/2015-04-22-admissions.md \
	$(INPUTDIR)/2015-04-22-censusevents.md \
	$(INPUTDIR)/2015-04-22-chartevents.md \
	$(INPUTDIR)/2015-04-22-cptevents.md \
	$(INPUTDIR)/2015-04-22-dcaregivers.md \
	$(INPUTDIR)/2015-04-22-dcareunits.md \
	$(INPUTDIR)/2015-04-22-ditems.md \
	$(INPUTDIR)/2015-04-22-dpatients.md \
	$(INPUTDIR)/2015-04-22-dunits.md \
	$(INPUTDIR)/2015-04-22-demographicdetail.md \
	$(INPUTDIR)/2015-04-22-drgevents.md \
	$(INPUTDIR)/2015-04-22-icd9.md \
	$(INPUTDIR)/2015-04-22-icustayevents.md \
	$(INPUTDIR)/2015-04-22-ioevents.md \
	$(INPUTDIR)/2015-04-22-labevents.md \
	$(INPUTDIR)/2015-04-22-lcpcomorbidityscores.md \
	$(INPUTDIR)/2015-04-22-lcpdailysapsi.md \
	$(INPUTDIR)/2015-04-22-lcpdailysofa.md \
	$(INPUTDIR)/2015-04-22-lcpelixhauserscores.md \
	$(INPUTDIR)/2015-04-22-lcpventilation.md \
	$(INPUTDIR)/2015-04-22-medevents.md \
	$(INPUTDIR)/2015-04-22-microbiologyevents.md \
	$(INPUTDIR)/2015-04-22-noteevents.md \
	$(INPUTDIR)/2015-04-22-orderentry.md \
	$(INPUTDIR)/2015-04-22-poemedorder.md \
	$(INPUTDIR)/2015-04-22-procedureevents.md \
	$(INPUTDIR)/2015-04-22-totalbalevents.md \
	-o $(OUTPUTDIR)/guidelines.pdf \
	-H $(STYLEDIR)/preamble.tex \
	--template $(STYLEDIR)/template.tex \
	-V fontsize=11pt \
	-V papersize=a4paper \
	-V documentclass:report \
	-N \
	--latex-engine=xelatex

docx:
	pandoc $(INPUTDIR)/*.md \
	-o $(OUTPUTDIR)/guidelines.docx \
	--csl=$(STYLEDIR)/ref_format.csl \
	--toc

.PHONY: help pdf docx
