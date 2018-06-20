#include "piinit.h"

void init_env() {
  symbol_car = mk_sym("CAR");
  symbol_cdr = mk_sym("CDR");
  symbol_cons = mk_sym("CONS");
  symbol_atom = mk_sym("ATOM");
  symbol_eq = mk_sym("EQ");
  symbol_true = mk_sym("T");
  symbol_addition = mk_sym("+");
  symbol_subtraction = mk_sym("-");
  symbol_multiplication = mk_sym("*");
  symbol_division = mk_sym("/");
  symbol_set = mk_sym("SET");
  symbol_lambda = mk_sym("LAMBDA");
  symbol_label = mk_sym("LABEL");
  symbol_quote = mk_sym("QUOTE");
  symbol_cond = mk_sym("COND");
  symbol_load = mk_sym("LOAD");
}

cell *load_env(char *init_file_path) {
  FILE *fp = fopen("init.lisp", "r");
  if(fp == NULL){
    printf("No init file found\n");
    return NULL;
  }
  return read_sexpr(fp);
}