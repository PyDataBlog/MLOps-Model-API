#ifndef SYMTAB_H
#define SYMTAB_H

#include "symbol.h"

void symtab_init();

void push_scope();
void pop_scope();

symbol *bind_symbol(char *name);
symbol *lookup_symbol(char *name);

void print_symtab();

#endif
