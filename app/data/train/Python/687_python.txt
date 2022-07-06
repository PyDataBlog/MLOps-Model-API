#
#Programa Lista 4, questão 1;
#Felipe Henrique Bastos Costa - 1615310032;
#
#
#
#


lista = []#lista vazia;
cont1 = 0#contador do indice;
cont2 = 1#contador da posição do numero, se é o primeiro, segundo etc;
v = 5#representaria o len da lista;

while(cont1 < v):
    x = int(input("Informe o %dº numero inteiro para colocar em sua lista:\n"%cont2))#x e a variavel que recebe
                                                                                     #o numero do usuario
    lista.append(x)#o numero informado para x e colocado dentro da lista;
    cont1+=1#Os contadores estao
    cont2+=1#sendo incrementados;
print("A lista de informada foi:\n%s"%lista)
