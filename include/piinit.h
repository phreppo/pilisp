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
cell *symbol_eq;		// eq
cell *symbol_eq_math; 	// =
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
cell *symbol_timer;
cell *symbol_or;
cell *symbol_and;
cell *symbol_not;
cell *symbol_greater;
cell *symbol_greater_equal;
cell *symbol_less;
cell *symbol_less_equal;
cell *symbol_length;
cell *symbol_member;
cell *symbol_nth;
cell *symbol_file_ended;
cell *symbol_env;


cell *GLOBAL_ENV;

void init_env();

cell *load_env(char *init_file_path);

#endif // !PIINIT_H