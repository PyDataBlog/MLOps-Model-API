# apidoc_sources = $(wildcard doc/api/*.markdown)
# apidocs = $(addprefix out/,$(apidoc_sources:.markdown=.html)) \
#           $(addprefix out/,$(apidoc_sources:.markdown=.json))

# apidoc_dirs = out/doc out/doc/api/ out/doc/api/assets

# apiassets = $(subst api_assets,api/assets,$(addprefix out/,$(wildcard doc/api_assets/*)))

# website_files = \
#     out/doc/sh_main.js    \
#     out/doc/sh_javascript.min.js

# doc: $(apidoc_dirs) $(website_files) $(apiassets) $(apidocs) tools/doc/ out/doc/changelog.html node

doc:

out/%.html: docs/%.md
	node ./generate.js --format=html --template=docs/template.html $< > $@

.PHONY: doc
