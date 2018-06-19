#ifndef PIBUILTIN_H
#define PIBUILTIN_H
#include "picell.h"
#include <stdbool.h>

bool atom(const cell *c);

cell *car(cell *c);
cell *cdr(cell *c);
cell *caar(cell *c);
cell *cddr(cell *c);
cell *cadr(cell *c);
cell *cdar(cell *c);

cell *cons(cell *car, cell *cdr);

cell *pairlis(cell *x, cell *y, cell *a);

// static inline cell* assoc(const cell* x, cell* l){
//   while (l){
//     if (eq(x,car(car(l)))) return l->car;
//     l=l->cdr;
//   }
//   return 0;
// }

#endif // !PIBUILTIN_H