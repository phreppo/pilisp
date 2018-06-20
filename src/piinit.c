#include "piinit.h"

void init_env() {
  symbol_car = mk_sym("car");
  symbol_cdr = mk_sym("cdr");
  symbol_cons = mk_sym("cons");
  symbol_atom = mk_sym("atom");
  symbol_eq = mk_sym("eq");
  symbol_true = mk_sym("T");
  symbol_addition = mk_sym("+");
  symbol_subtraction = mk_sym("-");
  symbol_multiplication = mk_sym("*");
  symbol_division = mk_sym("/");
}

cell *load_env(char *init_file_path) {
  FILE *fp = fopen("init.lisp", "r");
  if(fp == NULL){
    printf("No init file found\n");
    return NULL;
  }
  return read_sexpr(fp);
}