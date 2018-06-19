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
  return (v1 == v2);
}

cell *plus(const cell *numbers) {
  int result = 0;
  const cell *act = numbers;
  while (act) {
    if (!is_cons(act))
      pi_error(LISP_ERROR, "impossible to perform addition");
    if (!is_num(car(act)))
      pi_error(LISP_ERROR, "added a non-number");
    result += car(act)->value;
    act = cdr(act);
  }
  return mk_num(result);
}

cell *minus(const cell *numbers) {
  if (!numbers || !cdr(numbers))
    // we need 2 numbers at least
    pi_error_args();

  if (!is_cons(numbers) || !is_cons(cdr(numbers)))
    pi_error(LISP_ERROR, "impossible to perform subtraction");
  if (!is_num(car(numbers)) || !is_num(car(cdr(numbers))))
    pi_error(LISP_ERROR, "subtracted a non-number");
  int result = car(numbers)->value;
  const cell *act = cdr(numbers);
  while (act) {
    if (!is_cons(act))
      pi_error(LISP_ERROR, "impossible to perform subtraction");
    if (!is_num(car(act)))
      pi_error(LISP_ERROR, "subtracted a non-number");
    result -= car(act)->value;
    act = cdr(act);
  }
  return mk_num(result);
}

cell *multiplication(const cell *numbers) {
  int result = 1;
  const cell *act = numbers;
  while (act) {
    if (!is_cons(act))
      pi_error(LISP_ERROR, "impossible to perform multiplication");
    if (!is_num(car(act)))
      pi_error(LISP_ERROR, "multiplicated a non-number");
    result *= car(act)->value;
    act = cdr(act);
  }
  return mk_num(result);
}

cell *division(const cell *numbers) {
  if (!numbers || !cdr(numbers))
    // we need 2 numbers at least
    pi_error_args();

  if (!is_cons(numbers) || !is_cons(cdr(numbers)))
    pi_error(LISP_ERROR, "impossible to perform division");
  if (!is_num(car(numbers)) || !is_num(car(cdr(numbers))))
    pi_error(LISP_ERROR, "divided a non-number");
  double result = (double)car(numbers)->value;
  const cell *act = cdr(numbers);
  while (act) {
    if (!is_cons(act))
      pi_error(LISP_ERROR, "impossible to perform subtraction");
    if (!is_num(car(act)))
      pi_error(LISP_ERROR, "subtracted a non-number");
    if(car(act)->value == 0)
      pi_error(LISP_ERROR, "division for 0");
    result /= (double)car(act)->value;
    act = cdr(act);
  }
  return mk_num(result);
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
      if (eq(fn, symbol_atom)) {
        if (atom(car(x)))
          return symbol_true;
        else
          return NULL;
      }

      // EQ
      if (eq(fn, symbol_eq)) {
        if (eq(car(x), cadr(x)))
          return symbol_true;
        else
          return NULL;
      }

      // +
      if (eq(fn, symbol_plus)) {
        return plus(x);
      }

      // -
      if (eq(fn, symbol_minus)) {
        return minus(x);
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
    else {
      // it's a symbol: we have to search for that
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
      // quote function
      return cadr(e);
    // COND CASE HERE

    if (eq(car(e), mk_sym("LAMBDA"))) // lambda "autoquote"
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
