#ifndef PIBUILTIN_H
#define PIBUILTIN_H
#include "picell.h"
#include <stdbool.h>

bool atom(const cell *c);

bool eq(const cell* v1,const cell* v2);

cell *car(cell *c);
cell *cdr(cell *c);
cell *caar(cell *c);
cell *cddr(cell *c);
cell *cadr(cell *c);
cell *cdar(cell *c);

cell *cons(cell *car, cell *cdr);

cell *pairlis(cell *x, cell *y, cell *a);

cell* assoc(const cell* x, cell* l);

#endif // !PIBUILTIN_H