#ifndef CLIENTS_H_
#define CLIENTS_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "globals.h"

/* Inicialitza el stock inicial de la botiga
 * Utilitceu aquest funció per carregar els exemples
 **/
void inicialitzaClientsExemple(int exemple, client clients[C]);

/* Inicialitza els clients de la botiga a partir d'un fitxer.
 * Llegeix un fitxer que està identificat per filename i carrega les dades a la variable proveïdors
 * El fitxer conté un total de C línies on cada línia conté un enter i una direcció d'email vàlida
 * separats per el carácter ;
 *
 * Exemple d'una línia del fitxer de clients:
 * 1;botiga@c.com
 **/
void inicialitzaClientsArxiu(char* filename, client clients[C]);

/***
 * Retorna el email del client identificat per idClient dintre de la variable clients
 **/
char* emailClient(int idClient, client clients[P]);

#endif
