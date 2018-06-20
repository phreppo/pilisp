#include "picore.h"


cell *pairlis(cell *x, cell *y, cell *a) {
  // creates copies of everything
  cell *result = copy_cell(a);
  // ! UNSAFE: no checks about cell type
  while (x && y) {
    // if(atom(x) || atom(y))
    //   pi_error(LISP_ERROR,"pairlis error");
    cell *left = car(x);
    cell *right = car(y);
    cell *new_pair = mk_cons(copy_cell(left), copy_cell(right));
    result = mk_cons(new_pair, result);
    x = cdr(x);
    y = cdr(y);
  }
  return result;
}

cell *assoc(const cell *x, cell *l) {
  // ! UNSAFE
  while (l) {
    // we extract the first element in the pair
    if (eq(x, car(car(l))))
      // right pair
      return l->car;
    l = l->cdr;
  }
  return NULL;
}

cell *apply(cell *fn, cell *x, cell *a) {
  if (fn) {
    // puts("");
    // printf("Applying: ");
    // print_sexpr(fn);
    // printf(" to ");
    // print_sexpr(x);
    // puts("");
    if (atom(fn)) {

      // CAR
      if (eq(fn, symbol_car))
        return caar(x);

      // CDR
      if (eq(fn, symbol_cdr))
        return cdar(x);

      // CONS
      if (eq(fn, symbol_cons))
        return cons(car(x), cadr(x));

      // ATOM
      if (eq(fn, symbol_atom)) 
        if (atom(car(x)))
          return symbol_true;
        else
          return NULL;

      // T 
      if(eq(fn, symbol_true))
        return symbol_true;

      // EQ
      if (eq(fn, symbol_eq)) {
        if (eq(car(x), cadr(x)))
          return symbol_true;
        else
          return NULL;
      }

      // +
      if (eq(fn, symbol_addition)) {
        return addition(x);
      }

      // -
      if (eq(fn, symbol_subtraction)) {
        return subtraction(x);
      }

      // *
      if (eq(fn, symbol_multiplication)) {
        return multiplication(x);
      }

      // /
      if (eq(fn, symbol_division)) {
        return division(x);
      }

      // CUSTOM FUNCTION
      // does lambda exists?
      cell *function_body = eval(fn, a);
      if (function_body == NULL) {
        char *err = "unknown function ";
        char *fn_name = fn->sym;
        char *result = malloc(strlen(err) + strlen(fn_name) + 1);
        strcpy(result, err);
        strcat(result, fn_name);
        pi_error(LISP_ERROR, result);
      }
      // the env knows the lambda
      return apply(function_body, x, a);

    } else {

      // creating a lambda
      if (eq(car(fn), mk_sym("LAMBDA")))
        return eval(caddr(fn), pairlis(cadr(fn), x, a));
      
      // LABEL
      if (eq(car(fn), mk_sym("LABEL"))){
        return apply(
          caddr(fn),
          x,
          cons(cons(cadr(fn),caddr(fn)),a)
        );
      }
    }
  }
  return NULL; // error?
}

cell *eval(cell *e, cell *a) {
//   puts("");
//   printf("Evaluating:");
//   print_sexpr(e);
//   puts("");
  if (atom(e)) {
    if(!e)
      // NIL
      return NULL;
    if (is_num(e) || is_str(e))
      // it's a value
      return e;
    else {
      // it's a symbol: we have to search for that
      if(e == symbol_true)
        return symbol_true;
      cell *symbol_value = cdr(assoc(e, a));
      if (!symbol_value) {
        // the symbol has no value in the env
        char *err = "unknown symbol ";
        char *sym_name = e->sym;
        char *result = malloc(strlen(err) + strlen(sym_name) + 1);
        strcpy(result, err);
        strcat(result, sym_name);
        pi_error(LISP_ERROR, result);
      } else
        // the symbol has a value in the env
        return symbol_value;
    }
  }
  if (atom(car(e))) {
    // car of the cons cell is an atom

    if (eq(car(e), mk_sym("QUOTE")))
      // QUOTE
      return cadr(e);
    
    if(eq(car(e), mk_sym("COND")))
      // COND 
      return evcon(cdr(e),a);

    if (eq(car(e), mk_sym("LAMBDA"))) // lambda "autoquote"
      return e;

    // something else
    return apply(car(e), evlis(cdr(e), a), a);
  } else {
    // composed
    return apply(car(e), evlis(cdr(e), a), a);
  }
  pi_error(LISP_ERROR, "unable to evaluate expression");
}

cell *evlis(cell *m, cell *a) {
  if (!m)
    // empty par list
    return NULL;
  cell *valued_car = eval(car(m), a);
  cell *valued_cdr = evlis(cdr(m), a);
  return mk_cons(valued_car, valued_cdr);
}

cell * evcon(cell *c, cell *a){
  if(eval(caar(c),a) == symbol_true)
    return eval(cadar(c),a);
  else 
    return evcon(cdr(c),a);
}
