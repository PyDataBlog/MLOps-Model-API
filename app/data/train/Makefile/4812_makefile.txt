all: stdc_imp

stdc_imp: main.c
	cc -o stdc_imp main.c

.PHONY: clean

clean:
	@rm stdc_imp
