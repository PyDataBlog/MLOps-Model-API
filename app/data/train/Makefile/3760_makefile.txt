outfolder = report
all: report report/report.html $(outfolder)/report.md

$(outfolder):
	mkdir $(outfolder)

$(outfolder)/cover.md: cover.Rmd
	R -e "library(knitr); setwd('$(outfolder)');knit('../cover.Rmd')"

$(outfolder)/script1.md: script1.Rmd
	R -e "library(knitr); setwd('$(outfolder)');knit('../script1.Rmd')"

$(outfolder)/script2.md: script2.Rmd
	R -e "library(knitr); setwd('$(outfolder)');knit('../script2.Rmd')"

$(outfolder)/report.html: $(outfolder)/cover.md $(outfolder)/script1.md $(outfolder)/script2.md
	@echo "Converting md files to html"
	cd $(outfolder) && pandoc cover.md script1.md script2.md --toc -t html5 -s --template ../template.htm -o report.html


$(outfolder)/report.md: $(outfolder)/cover.md $(outfolder)/script1.md $(outfolder)/script2.md
	@echo "Converting md files to github md"
	cd $(outfolder) && pandoc cover.md script1.md script2.md -t markdown_github --toc -s -o report.md
