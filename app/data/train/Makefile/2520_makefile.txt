generalized_context:
	pandoc generalized_context.md -o generalized_context.pdf -f markdown --smart --reference-links --toc --filter pandoc-citeproc
