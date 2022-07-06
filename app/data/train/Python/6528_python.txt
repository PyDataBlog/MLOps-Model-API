#EQUIPE 2
#Nahan Trindade Passos - 1615310021
#Ana Beatriz Frota - 1615310027
#
#
#
#
#
#

import math 

print("Digite os termos da equacao ax2+bx+c")
a = float(input("Digite o valor de A:\n"))
if(a==0):
    print("Nao e uma equacao de segundo grau")
else:
    b = float(input("Valor de B:\n"))
    c = float(input("Valor de C:\n"))
    
    delta = (math.pow(b,2) - (4*a*c))
    
    if(delta<0):
        print("A equacao nao possui raizes reais")
    elif(delta == 0):
        raiz = ((-1)*b + math.sqrt(delta))/(2*a)
        print("A equacao possui apenas uma raiz",raiz)
    else:
        raiz1 = ((-1)*b + math.sqrt(delta))/(2*a)
        raiz2 = ((-1)*b - math.sqrt(delta))/(2*a)
        
        print("A equacao possui duas raizes")
        print("Primeira raiz:",raiz1)
        print("Segunda raiz:",raiz2)
        
