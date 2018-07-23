#include "piremove.h"

void cell_remove_lambda(cell * new_env, cell * old_env, cell * args, cell * fn){
  // FREE THINGS
  cell_remove_recursive(new_env->car->cdr);
  cell_remove_cars(args);                 // deep remove cars
  cell_remove_args(args);                 // remove args cons
  cell_remove_pairlis_deep(env, old_env); // remove associations
  unsafe_cell_remove(car(fn));            // function name
  cell_remove_recursive(cadr(fn));        // params
  unsafe_cell_remove(cddr(fn));           // cons pointing to body
  unsafe_cell_remove(cdr(fn));            // cons poining to param
  unsafe_cell_remove(fn);                 // cons pointing to lambda sym
}