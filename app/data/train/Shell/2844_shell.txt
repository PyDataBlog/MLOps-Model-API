#!/bin/bash
#BloquearDesbloquear
while [ m!=3 ]
do
clear	
	echo -e "\e[1;32m¿Que desea hacer?\n\e[0m"
	echo -e " [1] Bloquear Usuario"      
	echo -e " [2] Desbloquear Usuario"
	echo -e " [3] Volver" 
	echo -e -n "\n\e[0;32m>\e[0m" "Ingrese Opcion :"
read m
case $m in


1). BloquearUsuario.sh

;;

2). DesbloquearUsuario.sh


;;

3). ModificarUsuario.sh

;;


*)echo -e "\n\e[0;31m [x] Opcion incorrecta...\e[0m"
sleep 2
clear
. BloquearDesbloquear.sh

;;

esac
done
