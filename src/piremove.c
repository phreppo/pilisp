#include "piremove.h"

void cell_remove_lambda(cell *env, cell *old_env, cell *args, cell *fn) {
  cell_remove_recursive(env->car->cdr);
  cell_remove_cars(args);                 // deep remove cars
  cell_remove_args(args);                 // remove args cons
  cell_remove_pairlis_deep(env, old_env); // remove associations
  unsafe_cell_remove(car(fn));            // function name
  cell_remove_recursive(cadr(fn));        // params
  unsafe_cell_remove(cddr(fn));           // cons pointing to body
  unsafe_cell_remove(cdr(fn));            // cons poining to param
  unsafe_cell_remove(fn);                 // cons pointing to lambda sym
}

void cell_remove_eval_macro(cell *new_env, cell *old_env, cell *expression) {
  cell_remove_pairlis(new_env, old_env);
  cell_remove_recursive(cdr(expression));    // params tree
  unsafe_cell_remove(cdr(cdar(expression))); // cons of the body
  cell_remove_recursive(cadar(expression));  // formal params
  unsafe_cell_remove(cdar(expression));      // cons of params
  cell_remove(caar(expression));             // symbol macro
  unsafe_cell_remove(car(expression));       // cons of macro
  unsafe_cell_remove(expression);            // head of everything
}

void cell_remove_label(cell *new_env, cell *fn) {
  unsafe_cell_remove(cddr(fn));     // cons of the body
  unsafe_cell_remove(cadr(fn));     // symbol to bind
  unsafe_cell_remove(cdr(fn));      // cons of the top level
  unsafe_cell_remove(car(fn));      // symbol label
  unsafe_cell_remove(fn);           // cons of everything
  unsafe_cell_remove(car(new_env)); // new cons of the pair of the new env
  cell_remove(new_env);             // head of new env
}

void cell_remove_apply_macro(cell *env, cell *old_env, cell *args, cell *fn) {
  cell_remove_cars(args);                 // deep remove cars
  cell_remove_pairlis_deep(env, old_env); // remove associations
  unsafe_cell_remove(car(fn));            // function name
  cell_remove_recursive(cadr(fn));        // params
  unsafe_cell_remove(cddr(fn));           // cons pointing to body
  unsafe_cell_remove(cdr(fn));            // cons poining to param
  unsafe_cell_remove(fn);                 // cons pointing to lambda sym
}

void cell_remove_let_param(cell *params) {
  cell_remove(cdr(cdar(params)));
  cell_remove_recursive(cdar(params)); // maybe null
  unsafe_cell_remove(car(params));     // cons
  unsafe_cell_remove(params);
}