MAKEFLAGS = -rR

CXX_DIALECT = -std=c++14
CLANG_TIDY = clang-tidy

.SUFFIXES:

tidy: main.cc
	$(CLANG_TIDY) -checks=-*,modernize-* $< -- $(CXX_DIALECT)

fix: main.cc
	$(CLANG_TIDY) -checks=-*,modernize-* -fix $< -- $(CXX_DIALECT)
