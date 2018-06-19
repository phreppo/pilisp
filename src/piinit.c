#include "piinit.h"

void init_env(){
  symbol_car = mk_sym("car");
  symbol_cdr = mk_sym("cdr");
  symbol_cons = mk_sym("cons");
  symbol_atom = mk_sym("atom");
  symbol_eq = mk_sym("eq");
  symbol_true = mk_sym("T");
  symbol_plus = mk_sym("+");
  symbol_minus = mk_sym("-");
  symbol_multiplication = mk_sym("*");
  symbol_division = mk_sym("/");
}