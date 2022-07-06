.PHONY: all clean

all: dyn/main.js dyn/style.css

clean:
	@echo "==> Clean"
	rm -rf dyn/* .sass-cache

dyn/main.js: main.coffee
	@echo "==> dyn/main.js"
	coffee -c -o dyn main.coffee

dyn/style.css: style.scss
	@echo "==> dyn/style.css"
	sass --scss style.scss dyn/style.css
