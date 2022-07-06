# programme de test de l'achitecture minimale:
# test du chargement/ rangement
# test instruction logique
# test instruction de branchement
# test instruction arithmetique

## par bog le 6/12/14
#                                 NOMBRE DE 1
# Description du programme:
#	ce programme charge une valeur binaire en mémoire
#	puis compte le nombre de 1 puis l'enregistre en mémoire
#
##

#variable:
# 0($a0) -> adresse de la valeur
# 4($a0) -> adresse du nombre de 1
# $v0 -> valeur 
# $v1 -> nombre de 1
# $a3 -> taille d'un entier 32 bit

# initialisation des variables globales
lui $a0, 0x1001
addi $a3,$zero,0x20
#

#placement de la valeur en mémoire
addi $t0,$zero,42
sw $t0, 0($a0)
#

#chargement en mémoire de la valeur
lw $v0,0($a0)
addi $t0, $zero,0x20 #compteur de boucle 
add $t1, $zero,$zero #valeur du lsb
add $v1,$zero,$zero # compteur de 1
#

#calcul du nombre de 1
tantQue:
andi $t1,$v0,1
beq $t1, $zero,finSiUn
addi $v1, $v1, 1
finSiUn:
srl $v0, $v0, 1
addi $t0,$t0,-1
bgtz $t0 ,tantQue



#rangement du nombre de 1
sw $v1,4($a0)



 











	
