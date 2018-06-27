#include "piinit.h"

void init_builtin_lambdas(){
  builtin_lambda_index = 0;
  symbol_car = mk_builtin_lambda("CAR");
  symbol_cdr = mk_builtin_lambda("CDR");
  symbol_cons = mk_builtin_lambda("CONS");
  symbol_atom = mk_builtin_lambda("ATOM");
  symbol_eq = mk_builtin_lambda("EQ");
  symbol_eq_math = mk_builtin_lambda("=");
  symbol_true = mk_builtin_lambda("T");
  symbol_addition = mk_builtin_lambda("+");
  symbol_subtraction = mk_builtin_lambda("-");
  symbol_multiplication = mk_builtin_lambda("*");
  symbol_division = mk_builtin_lambda("/");
  symbol_set = mk_builtin_lambda("SET");
  symbol_lambda = mk_builtin_lambda("LAMBDA");
  symbol_label = mk_builtin_lambda("LABEL");
  symbol_quote = mk_builtin_lambda("QUOTE");
  symbol_cond = mk_builtin_lambda("COND");
  symbol_load = mk_builtin_lambda("LOAD");
  symbol_timer = mk_builtin_lambda("TIMER");
  symbol_or = mk_builtin_lambda("OR");
  symbol_and = mk_builtin_lambda("AND");
  symbol_not = mk_builtin_lambda("NOT");
  symbol_greater = mk_builtin_lambda(">");
  symbol_greater_equal = mk_builtin_lambda(">=");
  symbol_less = mk_builtin_lambda("<");
  symbol_less_equal = mk_builtin_lambda("<=");
  symbol_length = mk_builtin_lambda("LENGTH");
  symbol_member = mk_builtin_lambda("MEMBER");
  symbol_nth = mk_builtin_lambda("NTH");
  symbol_file_ended = mk_builtin_lambda("FEOF");
  symbol_env = mk_builtin_lambda("ENV");
  symbol_mem_dump = mk_builtin_lambda("MD");
  symbol_collect_garbage = mk_builtin_lambda("CG");
}

// adds to the list of the language symbols the new sym
static void add_language_symbol(cell *sym) {
  // cell *head = mk_cons(sym, LANGUAGE_SYMBOLS);
  // LANGUAGE_SYMBOLS = head;
}

void init_env() {
  // LANGUAGE_SYMBOLS = NULL;
  // symbol_car = mk_sym("CAR");
  // add_language_symbol(symbol_car);
  // symbol_cdr = mk_sym("CDR");
  // add_language_symbol(symbol_cdr);
  // symbol_cons = mk_sym("CONS");
  // add_language_symbol(symbol_cons);
  // symbol_atom = mk_sym("ATOM");
  // add_language_symbol(symbol_atom);
  // symbol_eq = mk_sym("EQ");
  // add_language_symbol(symbol_eq);
  // symbol_eq_math = mk_sym("=");
  // add_language_symbol(symbol_eq_math);
  // symbol_true = mk_sym("T");
  // add_language_symbol(symbol_true);
  // symbol_addition = mk_sym("+");
  // add_language_symbol(symbol_addition);
  // symbol_subtraction = mk_sym("-");
  // add_language_symbol(symbol_subtraction);
  // symbol_multiplication = mk_sym("*");
  // add_language_symbol(symbol_multiplication);
  // symbol_division = mk_sym("/");
  // add_language_symbol(symbol_division);
  // symbol_set = mk_sym("SET");
  // add_language_symbol(symbol_set);
  // symbol_lambda = mk_sym("LAMBDA");
  // add_language_symbol(symbol_lambda);
  // symbol_label = mk_sym("LABEL");
  // add_language_symbol(symbol_label);
  // symbol_quote = mk_sym("QUOTE");
  // add_language_symbol(symbol_quote);
  // symbol_cond = mk_sym("COND");
  // add_language_symbol(symbol_cond);
  // symbol_load = mk_sym("LOAD");
  // add_language_symbol(symbol_load);
  // symbol_timer = mk_sym("TIMER");
  // add_language_symbol(symbol_timer);
  // symbol_or = mk_sym("OR");
  // add_language_symbol(symbol_or);
  // symbol_and = mk_sym("AND");
  // add_language_symbol(symbol_and);
  // symbol_not = mk_sym("NOT");
  // add_language_symbol(symbol_not);
  // symbol_greater = mk_sym(">");
  // add_language_symbol(symbol_greater);
  // symbol_greater_equal = mk_sym(">=");
  // add_language_symbol(symbol_greater_equal);
  // symbol_less = mk_sym("<");
  // add_language_symbol(symbol_less);
  // symbol_less_equal = mk_sym("<=");
  // add_language_symbol(symbol_less_equal);
  // symbol_length = mk_sym("LENGTH");
  // add_language_symbol(symbol_length);
  // symbol_member = mk_sym("MEMBER");
  // add_language_symbol(symbol_member);
  // symbol_nth = mk_sym("NTH");
  // add_language_symbol(symbol_nth);
  // symbol_file_ended = mk_sym("FILE_ENDED");
  // add_language_symbol(symbol_file_ended);
  // symbol_env = mk_sym("ENV");
  // add_language_symbol(symbol_env);
  // symbol_mem_dump = mk_sym("MD");
  // add_language_symbol(symbol_mem_dump);
  // symbol_collect_garbage = mk_sym("CG");
  // add_language_symbol(symbol_collect_garbage);

  GLOBAL_ENV = mk_cons(mk_cons(mk_sym("p"), mk_str("a.lisp")), NULL);
}

void init_pi() {
  init_builtin_lambdas();
  init_memory();
  init_env();
  // cell_stack_push(memory->stack,LANGUAGE_SYMBOLS);
}

cell *load_env(char *init_file_path) {
  FILE *fp = fopen("init.lisp", "r");
  if (fp == NULL) {
    printf("No init file found\n");
    return NULL;
  }
  return read_sexpr(fp);
}