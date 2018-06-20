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
cell *symbol_set;
cell *symbol_addition;
cell *symbol_subtraction;
cell *symbol_multiplication;
cell *symbol_division;
cell *symbol_lambda;
cell *symbol_label;
cell *symbol_quote;
cell *symbol_cond;
cell *symbol_load;

void init_env();

cell * load_env(char * init_file_path);


#endif // !PIINIT_H