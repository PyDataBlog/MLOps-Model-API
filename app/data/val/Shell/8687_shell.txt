#!/usr/bin/env bash

# Le damos a gdb el sabor de intel para desensamblar código
echo "set disassembly-flavor intel" > ~/.gdbinit

# Creamos el directorio ~/bin. Se añade al PATH por el fichero ~/.profile
mkdir -p ~/bin

# Copiamos el fichero gcc2 a ~/bin y le damos permiso de ejecución
cp /vagrant/provisioning/gcc2 ~/bin
chmod 744 ~/bin/gcc2
