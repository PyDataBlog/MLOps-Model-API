SHELL := /bin/bash

LATEX := lualatex
LATEXFILE := poster-template.tex


.PHONY: compile
compile:
	$(LATEX) $(LATEXFILE)

.PHONY: watch
watch:
	@command -v inotifywait >/dev/null 2>&1 || { \
		echo >&2 "Please consider installing inotify-tools first. Aborting."; \
		exit 1; \
	}
	@echo "--> Watching following files for changes..."
	@find . -name '*.tex'

	@inotifywait -q -m -e modify `find . -name '*.tex'` | \
		while read -r filename event; do \
		  make compile; \
			echo "\n\n\n--> Watching following files for changes..."; \
			find . -name '*.tex'; \
		done
