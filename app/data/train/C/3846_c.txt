#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#ifndef _lista_simples_h
#define _lista_simples_h

/*A implementacao utiliza um noh cabeca apontando
 * para o primeiro elemento da lista */

// Define um ponteiro void para dados genericos
typedef void* gpointer;
// Definicao estrutura lista
typedef struct noh noh;
typedef noh *lista;
struct noh {
    gpointer elem;
    noh *prox;
};

typedef int (*cmp_func)(gpointer a, gpointer b);

// Posicao de um elemento é um ponteiro que contem
// o antecessor desse elemento
typedef noh *posicao;

// Declaraçao Operadores

bool inic_lista(lista *L);
bool inserir_no_fim(lista *L, gpointer valor);
bool inserir_no_comeco(lista *L, gpointer valor);
bool inserir(lista *L, posicao p, gpointer valor);
bool inserir_ordenado(lista *L, gpointer valor, cmp_func func);
gpointer deletar(lista *L, posicao p);
posicao local(lista L, gpointer valor);
bool esvaziar (lista *L);
posicao primeira(lista L);
posicao fim(lista L);
posicao proxima(lista L);
gpointer elemento(Lista L, posicao);

// Funcoes auxiliares

bool alocar_noh(noh **no, gpointer dados); 



#endif
