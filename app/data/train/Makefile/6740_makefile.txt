# Fado Pagem
#
# Joan Josep Ordinas Rosa <jordinas@gmail.com> 
#

FADO=pagem

.PHONY : all clean build backup zip

LILYPOND=lilypond -dno-point-and-click \
		  -ddelete-intermediate-files \
		  -drelative-includes

ROMAN=biblioteca/roman.ily
PAGINA=biblioteca/pagina.ily

ANALISE=musica/analise.ily
BORDOES=musica/bordoes.ily
CONTRA=musica/contra.ily
HARMONIA=musica/harmonia.ily
LETRA=musica/letra.ily
MELODIA=musica/melodia.ily

#all: fake.pdf voz.pdf viola.pdf tablatura.pdf conduzir.pdf
all: fake.pdf voz.pdf
	@echo 'Bem feito!'

conduzir.pdf: conduzir.ly $(PAGINA) $(ANALISE) $(HARMONIA) $(LETRA) $(MELODIA) 
	@$(LILYPOND) $<

voz.pdf: voz.ly $(PAGINA) $(LETRA) $(MELODIA)
	@$(LILYPOND) $<

fake.pdf: fake.ly $(PAGINA) $(HARMONIA) $(MELODIA) 
	@$(LILYPOND) $<

viola.pdf: viola.ly $(PAGINA) $(ANALISE) $(BORDOES) $(CONTRA) $(HARMONIA) $(MELODIA) 
	@$(LILYPOND) $<

tablatura.pdf: tablatura.ly $(PAGINA) $(BORDOES) $(CONTRA) $(HARMONIA) $(MELODIA) 
	@$(LILYPOND) $<

clean:
	@rm -f *.ps *.pdf *.log

build: clean all

lily:
	cd ..; zip -v -r /tmp/$(FADO).zip $(FADO)/* -x '*.pdf'

zip:
	@zip -q -r /tmp/$(FADO).zip *

backup: all zip
	@gmail /tmp/$(FADO).zip

# EOF
