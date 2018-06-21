#include "piinit.h"

void init_env() {
  symbol_car = mk_sym("CAR");
  symbol_cdr = mk_sym("CDR");
  symbol_cons = mk_sym("CONS");
  symbol_atom = mk_sym("ATOM");
  symbol_eq = mk_sym("EQ");
  symbol_eq = mk_sym("=");
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
  symbol_timer = mk_sym("TIMER");
  symbol_or = mk_sym("OR");
  symbol_and = mk_sym("AND");
  symbol_not = mk_sym("NOT");
  symbol_greater= mk_sym(">");
  symbol_greater_equal= mk_sym(">=");
  symbol_less= mk_sym("<");
  symbol_less_equal= mk_sym("<=");
  symbol_length= mk_sym("length");
  symbol_member= mk_sym("member");
  symbol_nth= mk_sym("nth");
}

cell *load_env(char *init_file_path) {
  FILE *fp = fopen("init.lisp", "r");
  if(fp == NULL){
    printf("No init file found\n");
    return NULL;
  }
  return read_sexpr(fp);
}