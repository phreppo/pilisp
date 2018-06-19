#include "pibuiltin.h"
#include "pierror.h"

bool atom(const cell *c) {
  return (c == NULL) // NIL case
         || (c->type == TYPE_SYM || c->type == TYPE_NUM || c->type == TYPE_STR);
}

// ! UNSAFE FUNCTIONS

cell *car(cell *c) {
  if (c == NULL)
    // (car NIL)
    return NULL;
  // if(atom(c))
  //     // (car 1)
  //     // TODO: questo è sbagliato, potrebbe essere un simbolo che sta per una
  //     stringa pi_error(LISP_ERROR, "car applied to atom");
  if (c) {
    // unsafe
    return c->car;
  } else
    // empty cell
    return NULL;
}

cell *caar(cell *c) { return car(car(c)); }

cell *cdr(cell *c) {
  if (c == NULL)
    // (car NIL)
    return NULL;

  return c->cdr;
}

cell *cddr(cell *c) { return cdr(cdr(c)); }

cell *cadr(cell *c) { return car(cdr(c)); }

cell *cdar(cell *c) { return car(car(c)); }

cell *cons(cell *car, cell *cdr) { return mk_cons(car, cdr); }

cell* pairlis(cell* x, cell* y,cell* a) {
  // creates copies of everything
  cell * result = copy_cell(a);
  // ! TOTAL UNSAFE: no checks about cell type
  while(x && y){
    // if(atom(x) || atom(y))
    //   pi_error(LISP_ERROR,"pairlis error");
    cell * left = car(x);
    cell * right = car(y);
    cell * new_pair = mk_cons(copy_cell(left),copy_cell(right));
    result = mk_cons(new_pair,result);
    x = cdr(x);
    y = cdr(y);
  }
  return result;
}
