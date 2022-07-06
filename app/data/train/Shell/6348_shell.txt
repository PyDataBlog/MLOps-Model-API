#!/bin/bash
#ModificarUsuario
while [ m!=5 ]
do
clear
	echo -e " \e[0;34m____________________________\e[0m" 
	echo -e "\e[0;34m|\e[0m"	"     Modificar Usuario"	"    \e[0;34m|\e[0m"
	echo -e "\e[0;34m|¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯|\e[0m"
	echo -e "\e[0;34m|\e[0m"	" [1] Nombre de Usuario"      "    \e[0;34m|\e[0m"
	echo -e "\e[0;34m|\e[0m"	" [2] Grupo\t\t"      "    \e[0;34m|\e[0m"
	echo -e "\e[0;34m|\e[0m"	" [3] Comentario\t"      "    \e[0;34m|\e[0m"
	echo -e "\e[0;34m|\e[0m"	" [4] Bloquear/Desbloquear"      " \e[0;34m|\e[0m"
	echo -e "\e[0;34m|\e[0m"	" [5] Opciones Avanzadas"      "   \e[0;34m|\e[0m"
	echo -e "\e[0;34m|\e[0m"	" [6] Atras\t\t"      "    \e[0;34m|\e[0m"
	echo -e "\e[0;34m|____________________________|\e[0m"
	echo -e -n "\n\e[0;32m>\e[0m" "Ingrese Opcion :"
read m
case $m in

1). ModificarUsuarioLogin.sh
;;
2). ModificarUsuarioG.sh
;;
3). ModificarUsuarioComentario.sh
;;
4). BloquearDesbloquear.sh
;;
5). ModificarUsuario2.sh 
;;
6)break
;;
*)echo -e "\n\e[0;31m [x] Opcion incorrecta...\e[0m"
sleep 2
clear
;;
esac
done
