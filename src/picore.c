#include "picore.h"

cell *pairlis(cell *x, cell *y, cell **a) {
  // creates copies of everything
  // puts("");
  // printf("Pairlis on: ");
  // print_sexpr(*a);
  // puts("");
  cell *result = copy_cell(*a);
  // ! UNSAFE: no checks about cell type
  while (x) {
    // if(atom(x) || atom(y))
    //   pi_error(LISP_ERROR,"pairlis error");
    cell *left = car(x);
    cell *right = car(y);
    cell *new_pair = mk_cons(copy_cell(left), copy_cell(right));
    result = mk_cons(new_pair, result);
    x = cdr(x);
    y = cdr(y);
  }
  // printf("Pairlis now: ");
  // print_sexpr(result);
  // puts("");
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

cell *apply(cell *fn, cell *x, cell **a) {
  if (fn) {
    // puts("");
    // printf("Applying: ");
    // print_sexpr(fn);
    // printf(" to ");
    // print_sexpr(x);
    // puts("");
    if (atom(fn)) {

      // BASIC OPERATIONS
      if (eq(fn, symbol_car))
        return caar(x);
      if (eq(fn, symbol_cdr))
        return cdar(x);
      if (eq(fn, symbol_cons))
        return cons(car(x), cadr(x));
      if (eq(fn, symbol_atom))
        if (atom(car(x)))
          return symbol_true;
        else
          return NULL;
      if (eq(fn, symbol_true))
        return symbol_true;
      if (eq(fn, symbol_eq) || eq(fn, symbol_eq_math)) {
        if (eq(car(x), cadr(x)))
          return symbol_true;
        else
          return NULL;
      }

      // UTILITY
      if (eq(fn, symbol_set))
        return set(car(x), cadr(x), a);
      if (eq(fn, symbol_load))
        return load(car(x), a);
      if (eq(fn, symbol_timer))
        return timer(car(x), a);

      // ARITHMETIC OPERATORS
      if (eq(fn, symbol_addition))
        return addition(x);
      if (eq(fn, symbol_subtraction))
        return subtraction(x);
      if (eq(fn, symbol_multiplication))
        return multiplication(x);
      if (eq(fn, symbol_division))
        return division(x);

      // LOGICAL OPERATORS
      if (eq(fn, symbol_or))
        return or (x);
      if (eq(fn, symbol_and))
        return and(x);
      if (eq(fn, symbol_not))
        return not(x);

      // COMPARISON OPERATORS
      if (eq(fn, symbol_greater))
        return greater(x);
      if (eq(fn, symbol_greater_equal))
        return greater_eq(x);
      if (eq(fn, symbol_less))
        return less(x);
      if (eq(fn, symbol_less_equal))
        return less_eq(x);

      // LISTS
      if (eq(fn, symbol_length))
        return length(x);
      if (eq(fn, symbol_member))
        return member(x);
      if (eq(fn, symbol_nth))
        return nth(x);

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
      if (!is_cons(function_body))
        pi_error(LISP_ERROR, "trying to apply a non-function");
      
      // the env knows the lambda
      return apply(function_body, x, a);

    } else {

      // APLLYING A LAMBDA
      if (eq(car(fn), symbol_lambda)) {
        cell *env = pairlis(cadr(fn), x, a);
        return eval(caddr(fn), &env);
      }

      // LABEL
      if (eq(car(fn), symbol_label))
        return apply(caddr(fn), x, cons(cons(cadr(fn), caddr(fn)), a));
    }
  }
  return NULL; // error?
}

cell *eval(cell *e, cell **a) {
  // puts("");
  // printf("Evaluating:");
  // print_sexpr(e);
  // puts("");
  if (atom(e)) {
    if (!e)
      // NIL
      return NULL;
    if (is_num(e) || is_str(e))
      // VALUE
      return e;
    else {
      // it's a symbol: we have to search for that
      if (e == symbol_true)
        return symbol_true;
      cell *pair = assoc(e, *a);
      cell *symbol_value = cdr(pair);
      if (!pair) {
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

    if (eq(car(e), symbol_quote))
      // QUOTE
      return cadr(e);

    if (eq(car(e), symbol_cond))
      // COND
      return evcon(cdr(e), a);

    if (eq(car(e), symbol_lambda))
      // lambda "autoquote"
      return e;

    // something else
    return apply(car(e), evlis(cdr(e), a), a);

  } else
    // composed
    return apply(car(e), evlis(cdr(e), a), a);
  pi_error(LISP_ERROR, "unable to evaluate expression");
}

cell *evlis(cell *m, cell **a) {
  if (!m)
    // empty par list
    return NULL;
  cell *valued_car = eval(car(m), a);
  cell *valued_cdr = evlis(cdr(m), a);
  return mk_cons(valued_car, valued_cdr);
}

cell *evcon(cell *c, cell **a) {
  if (eval(caar(c), a) != NULL)
    return eval(cadar(c), a);
  else
    return evcon(cdr(c), a);
}
