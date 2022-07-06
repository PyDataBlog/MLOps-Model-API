# Makefile to build our databases

db: armour-db.tex

armour-db.tex: armour.db db2tex
	./db2tex < $< > $@


