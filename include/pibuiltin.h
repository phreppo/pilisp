#ifndef PIBUILTIN_H
#define PIBUILTIN_H
#include "picell.h"
#include "pilisp.h"
#include <stdbool.h>
#include <time.h>

cell *car(const cell *c);
cell *cdr(const cell *c);
cell *caar(const cell *c);
cell *cddr(const cell *c);
cell *cadr(const cell *c);
cell *cdar(const cell *c);
cell *cadar(const cell *c);
cell *caddr(const cell *c);
cell *cons(const cell *car, const cell *cdr);
int atom(const cell *c);

bool eq(const cell *v1, const cell *v2);

cell *or(const cell * operands);
cell *and(const cell * operands);
cell *not(const cell * operands);

cell *set(cell *name, cell *val, cell **env);
cell *load(cell *name, cell **env);

cell *addition(const cell *numbers);
cell *subtraction(const cell *numbers);
cell *multiplication(const cell *numbers);
cell *division(const cell *numbers);

cell * timer(const cell *to_execute, cell **env);

#endif // !PIBUILTIN_H