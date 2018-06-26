#include "piinit.h"

// adds to the list of the language symbols the new sym
static void add_language_symbol(cell *sym) {
  cell *head = mk_cons(sym, LANGUAGE_SYMBOLS);
  LANGUAGE_SYMBOLS = head;
}

void init_env() {
  LANGUAGE_SYMBOLS = NULL;
  symbol_car = mk_sym("CAR");
  symbol_cdr = mk_sym("CDR");
  symbol_cons = mk_sym("CONS");
  symbol_atom = mk_sym("ATOM");
  symbol_eq = mk_sym("EQ");
  symbol_eq_math = mk_sym("=");
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
  symbol_greater = mk_sym(">");
  symbol_greater_equal = mk_sym(">=");
  symbol_less = mk_sym("<");
  symbol_less_equal = mk_sym("<=");
  symbol_length = mk_sym("LENGTH");
  symbol_member = mk_sym("MEMBER");
  symbol_nth = mk_sym("NTH");
  symbol_file_ended = mk_sym("FILE_ENDED");
  symbol_env = mk_sym("ENV");
  symbol_mem_dump = mk_sym("MD");
  symbol_collect_garbage = mk_sym("CG");

  add_language_symbol(symbol_car);
  add_language_symbol(symbol_cdr);
  add_language_symbol(symbol_cons);
  add_language_symbol(symbol_atom);
  add_language_symbol(symbol_eq);
  add_language_symbol(symbol_eq_math);
  add_language_symbol(symbol_true);
  add_language_symbol(symbol_addition);
  add_language_symbol(symbol_subtraction);
  add_language_symbol(symbol_multiplication);
  add_language_symbol(symbol_division);
  add_language_symbol(symbol_set);
  add_language_symbol(symbol_lambda);
  add_language_symbol(symbol_label);
  add_language_symbol(symbol_quote);
  add_language_symbol(symbol_cond);
  add_language_symbol(symbol_load);
  add_language_symbol(symbol_timer);
  add_language_symbol(symbol_or);
  add_language_symbol(symbol_and);
  add_language_symbol(symbol_not);
  add_language_symbol(symbol_greater);
  add_language_symbol(symbol_greater_equal);
  add_language_symbol(symbol_less);
  add_language_symbol(symbol_less_equal);
  add_language_symbol(symbol_length);
  add_language_symbol(symbol_member);
  add_language_symbol(symbol_nth);
  add_language_symbol(symbol_file_ended);
  add_language_symbol(symbol_env);
  add_language_symbol(symbol_mem_dump);
  add_language_symbol(symbol_collect_garbage);

  GLOBAL_ENV = mk_cons(mk_cons(mk_sym("p"), mk_str("a.lisp")), NULL);
}

void init_pi() {
  init_memory();
  init_env();
}

cell *load_env(char *init_file_path) {
  FILE *fp = fopen("init.lisp", "r");
  if (fp == NULL) {
    printf("No init file found\n");
    return NULL;
  }
  return read_sexpr(fp);
}