#include "picore.h"

cell * last_pairlis = NULL;

cell *pairlis(cell *x, cell *y, cell *a) {
#if DEBUG_MODE
  printf("Pairlis:\t" ANSI_COLOR_GREEN);
  print_sexpr(x);
  printf(ANSI_COLOR_RESET " wiht: " ANSI_COLOR_GREEN);
  print_sexpr(y);
  printf(ANSI_COLOR_RESET " in the env " ANSI_COLOR_RED);
  print_sexpr(a);
  printf(ANSI_COLOR_RESET "\n");
#endif
  cell *result = a;
  // ! UNSAFE: no checks about cell type
  while (x) {
    cell *left = car(x);
    cell *right = car(y);
    cell *new_pair = mk_cons(left, right);
    result = mk_cons(new_pair, result);
    x = cdr(x);
    y = cdr(y);
  }
#if DEBUG_MODE
  printf("Parilis res:\t" ANSI_COLOR_BLUE);
  print_sexpr(result);
  printf(ANSI_COLOR_RESET "\n");
#endif
  last_pairlis = result;
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
#if DEBUG_MODE
  printf("Applying:\t" ANSI_COLOR_GREEN);
  print_sexpr(fn);
  printf(ANSI_COLOR_RESET " to: " ANSI_COLOR_BLUE);
  print_sexpr(x);
  printf(ANSI_COLOR_RESET " in the env: " ANSI_COLOR_RED);
  print_sexpr(a);
  printf(ANSI_COLOR_RESET "\n");
#endif
  if (fn) {
    if (atom(fn)) {

      // BASIC OPERATIONS
      if (eq(fn, symbol_car))
        return caar(x);
      if (eq(fn, symbol_cdr))
        return cdar(x);
      if (eq(fn, symbol_cons))
        return cons(car(x), cadr(x));
      if (eq(fn, symbol_atom)) {
        if (atom(car(x)))
          return symbol_true;
        else
          return NULL;
      }
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
        pi_error(LISP_ERROR, "trying to apply a non-lambda");

      // the env knows the lambda
      return apply(function_body, x, a);

    } else {
      // composed function
      if (eq(car(fn), symbol_lambda)) {
        // direct lambda
#if DEBUG_MODE
        printf("LAMBDA:\t\t" ANSI_COLOR_RED);
        print_sexpr(fn);
        printf(ANSI_COLOR_RESET "\n");
#endif
        a = pairlis(cadr(fn), x, a);
        return eval(caddr(fn), a);
      }
      // LABEL
      if (eq(car(fn), symbol_label)) {
        cell *new_env = cons(cons(cadr(fn), caddr(fn)), a);
        return apply(caddr(fn), x, new_env);
      }

#if DEBUG_MODE
      printf("Resolving fun: \t" ANSI_COLOR_RED);
      print_sexpr(fn);
      printf(ANSI_COLOR_RESET "\n");
#endif
      // function is not an atomic function: something like (lambda (x) (lambda
      // (y) y)) ! cell * new_env = pairlis(,a)
      // ! qui devo ricordarmi dell'ambiente interno (?)
      cell *function_body = eval(fn, a);
      a = last_pairlis; // ?

      if (function_body == NULL) {
        char *err = "unknown function ";
        char *fn_name = fn->sym;
        char *result = malloc(strlen(err) + strlen(fn_name) + 1);
        strcpy(result, err);
        strcat(result, fn_name);
        pi_error(LISP_ERROR, result);
      }
      if (!is_cons(function_body))
        pi_error(LISP_ERROR, "trying to apply a non-lambda");
      // the env knows the lambda
      return apply(function_body, x, a);
    }
  }
  return NULL; // error?
}

cell *eval(cell *e, cell *a) {
#if DEBUG_MODE
  printf("Evaluating: \t" ANSI_COLOR_GREEN);
  print_sexpr(e);
  printf(ANSI_COLOR_RESET " in the env: " ANSI_COLOR_RED);
  print_sexpr(a);
  printf(ANSI_COLOR_RESET "\n");
#endif
  cell *evaulated = NULL;
  if (atom(e)) {
    if (!e)
      // NIL
      evaulated = NULL;
    else {
      if (is_num(e) || is_str(e))
        // VALUE
        evaulated = e;
      else {
        // it's a symbol: we have to search for that
        if (e == symbol_true)
          evaulated = symbol_true;
        else {
          cell *pair = assoc(e, a);
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
            evaulated = symbol_value;
        }
      }
    }
  } else if (atom(car(e))) {
    // car of the cons cell is an atom

    if (eq(car(e), symbol_quote))
      // QUOTE
      evaulated = cadr(e);
    else {

      if (eq(car(e), symbol_cond))
        // COND
        evaulated = evcon(cdr(e), a);
      else {

        if (eq(car(e), symbol_lambda))
          // lambda "autoquote"
          evaulated = e;
        else {
          // something else
          evaulated = apply(car(e), evlis(cdr(e), a), a);
        }
      }
    }

  } else {
    // ! composed function
    evaulated = apply(car(e), evlis(cdr(e), a), a);
  }
#if DEBUG_MODE
  printf("Evaluated: \t" ANSI_COLOR_GREEN);
  print_sexpr(e);
  printf(ANSI_COLOR_RESET " to: " ANSI_COLOR_RED);
  print_sexpr(evaulated);
  printf(ANSI_COLOR_RESET "\n");
#endif
  return evaulated;
}

cell *evlis(cell *m, cell *a) {
#if DEBUG_MODE
  printf("Evlis: \t\t" ANSI_COLOR_GREEN);
  print_sexpr(m);
  printf(ANSI_COLOR_RESET);
  puts("");
#endif
  if (!m)
    // empty par list
    return NULL;
  cell *valued_car = eval(car(m), a);
  cell *valued_cdr = evlis(cdr(m), a);
  return mk_cons(valued_car, valued_cdr);
}

cell *evcon(cell *c, cell *a) {
  if (eval(caar(c), a) != NULL)
    return eval(cadar(c), a);
  else
    return evcon(cdr(c), a);
}
