#include "picell.h"


cell *get_cell() { return (cell *)malloc(sizeof(cell)); }

cell *mk_num(int n) {
  cell *c = get_cell();
  c->type = TYPE_NUM;
  c->value = n;
  return c;
}

cell *mk_str(const char *s) {
  cell *c = get_cell();
  c->type = TYPE_STR;
  c->str = malloc(strlen(s) + 1);
  strcpy(c->str, s);
  return c;
}

cell *mk_sym(const char *symbol) {
  cell *c = get_cell();
  c->type = TYPE_SYM;
  c->str = malloc(strlen(symbol) + 1);
  strcpy(c->str, symbol);
  return c;
}
