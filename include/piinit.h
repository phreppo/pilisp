#ifndef PIINIT_H
#define PIINIT_H 
#include "picell.h"

cell *symbol_car;
cell *symbol_cdr;
cell *symbol_cons;
cell *symbol_atom;
cell *symbol_eq;
cell *symbol_true;
cell *symbol_plus;
cell *symbol_minus;
cell *symbol_multiplication;
cell *symbol_division;

void init_env();


#endif // !PIINIT_H