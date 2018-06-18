#ifndef PIBUILTIN_H
#define PIBUILTIN_H
#include <stdbool.h>
#include "picell.h"

bool atom(const cell * c);

cell * car(cell * c);
cell * cdr(cell * c);
cell * caar(cell * c);
cell * cadr(cell * c);
cell * cdar(cell * c);

cell * cons(cell * car, cell * cdr);

// TODO riparti dalle prossime due

// static inline cell* pairlis(cell* x, cell* y,cell* a) {
//   // ATTENZIONE: questa versione gira la lista X Y Z che viene messa nell' ambiente come Z Y X
//   cell* res=push(a);
//   while(x){
//     if (ATOM(x)){
//       res=swp(mk_cons(mk_cons(x,y),res));
//       x=y=0;
//     } else {
//       CHECK_S(!y,LISP_ERROR,"\"%s\": too few arguments",curr_fn->sym);
//       res=swp(mk_cons(mk_cons(car(x),car(y)),res));
//       x=x->cdr;
//       y=y->cdr;
//     }
//   }
//   CHECK_S(y,LISP_ERROR,"\"%s\": too many arguments",curr_fn->sym);
//   return pop(res);
// }

// static inline cell* assoc(const cell* x, cell* l){
//   while (l){
//     if (eq(x,car(car(l)))) return l->car;
//     l=l->cdr;
//   }
//   return 0;
// }


#endif // !PIBUILTIN_H