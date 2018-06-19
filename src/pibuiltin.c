#include "pibuiltin.h"
#include "pierror.h"

int atom(const cell *c) {
  return (c == NULL) // NIL case
         || (c->type == TYPE_SYM || c->type == TYPE_NUM || c->type == TYPE_STR);
}

bool eq(const cell *v1, const cell *v2) {
  if (!v1 || !v2)
    return (v1 == v2);
  if (is_num(v1) && is_num(v2))
    return (v1->value == v2->value);
  if (is_str(v1) && is_str(v2))
    return (strcmp(v1->str, v2->str) == 0);

  // ! NEW: for now we have not symbol table
  if (is_sym(v1) && is_sym(v2))
    return (strcmp(v1->sym, v2->sym) == 0);
  return (v1 == v2);
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

cell *cdar(cell *c) { return cdr(car(c)); }

cell *caddr(cell *c) { return car(cdr(cdr(c))); }

cell *cons(cell *car, cell *cdr) { return mk_cons(car, cdr); }

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
    puts("");
    printf("Applying: ");
    print_sexpr(fn);
    printf(" to ");
    print_sexpr(x);
    puts("");
    if (atom(fn)) {
      if (eq(fn, mk_sym("CAR")))
        return caar(x);
      if (eq(fn, mk_sym("CDR")))
        return cdar(x);
      if (eq(fn, mk_sym("CONS")))
        return cons(car(x), cadr(x));
      if (eq(fn, mk_sym("ATOM")))
        // not working: how to represent T?
        // return atom(car(x));
        return NULL;
      if (eq(fn, mk_sym("EQ")))
        // not working: (eq a a) = NIL
        return NULL;
      // return eq(car(x),cadr(x));
      // return eq(car(x),cadr(x));
      // TODO call custom lambdas
      // lambda exists?
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
      if (eq(car(fn), mk_sym("LAMBDA")))
        // he's creating a lambda
        return eval(caddr(fn), pairlis(cadr(fn), x, a));
      // LABEL CASE HERE
    }
  }
  return NULL; // error?
}

cell *eval(cell *e, cell *a) {
  puts("");
  printf("Evaluating:");
  print_sexpr(e);
  puts("");
  if (atom(e)) {
    if (is_num(e) || is_str(e))
      // it's a value
      return e;
    else
      // it's a symbol: we have to search for that
      return cdr(assoc(e, a));
  }
  if (atom(car(e))) {
    // car of the cons cell is an atom
    if (eq(car(e), mk_sym("QUOTE")))
      // quote function
      return cadr(e);
    // COND CASE HERE

    if( eq(car(e), mk_sym("LAMBDA"))) // lambda "autoquote"
      return e;
    return apply(car(e), evlis(cdr(e), a), a);
  } else {
    // composed
    return apply(car(e), evlis(cdr(e), a), a);
  }
  pi_error(LISP_ERROR, "Unable to evaluate expression");
}

cell *evlis(cell *m, cell *a) {
  if (!m)
    // empty par list
    return NULL;
  cell *valued_car = eval(car(m), a);
  cell *valued_cdr = evlis(cdr(m), a);
  return mk_cons(valued_car, valued_cdr);
}
