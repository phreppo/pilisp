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
