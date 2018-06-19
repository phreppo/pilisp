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
  return NULL;
  // cell* res=push(a);
  // while(x){
  //   if (ATOM(x)){
  //     res=swp(mk_cons(mk_cons(x,y),res));
  //     x=y=0;
  //   } else {
  //     CHECK_S(!y,LISP_ERROR,"\"%s\": too few arguments",curr_fn->sym);
  //     res=swp(mk_cons(mk_cons(car(x),car(y)),res));
  //     x=x->cdr;
  //     y=y->cdr;
  //   }
  // }
  // CHECK_S(y,LISP_ERROR,"\"%s\": too many arguments",curr_fn->sym);
  // return pop(res);
}
