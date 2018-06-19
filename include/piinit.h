#ifndef PIINIT_H
#define PIINIT_H 
#include "picell.h"
#include "piparser.h"
#include <stdio.h>
#define INIT_FILE_PATH_GLOBAL "../init.lisp"

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

cell * load_env(char * init_file_path);


#endif // !PIINIT_H