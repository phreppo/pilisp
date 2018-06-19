#ifndef PIBUILTIN_H
#define PIBUILTIN_H
#include "picell.h"
#include "pilisp.h"
#include <stdbool.h>

int atom(const cell *c);

bool eq(const cell* v1,const cell* v2);

cell * plus(const cell * numbers);
cell * minus(const cell * numbers);
cell * multiplication(const cell * numbers);
cell * division(const cell * numbers);


cell *car(cell *c);
cell *cdr(cell *c);
cell *caar(cell *c);
cell *cddr(cell *c);
cell *cadr(cell *c);
cell *cdar(cell *c);
cell *caddr(cell *c);

cell *cons(cell *car, cell *cdr);


#endif // !PIBUILTIN_H