# Colores
N=[0m
V=[01;32m
A=[01;33m

all:
	@echo ""
	@echo "Comandos disponibles"
	@echo ""
	@echo "  $(A)De uso para desarrollo: $(N)"
	@echo ""
	@echo "  $(V)iniciar$(N)    Instala todas las dependencias."
	@echo "  $(V)build$(N)      Compila todo (ver GruntFile.js)."
	@echo "  $(V)test$(N)       Ejecutar la aplicación (require el comando nwjs)."
	@echo ""


iniciar:
	npm install
	bower install

build:
	grunt nodewebkit
	@echo ""
	@echo "$(A)Listo, los archivos generados están en ./webkitbuilds/ $(N)"
	@echo ""


test:
	nwjs src

test_mac:
	open -a /Applications/nwjs.app --args /Users/hugoruscitti/proyectos/datalogger/src
