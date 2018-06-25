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

cell *addition(const cell *numbers) {
  long result = 0;
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

cell *subtraction(const cell *numbers) {
  if (!numbers)
    // we need 1 argument at least
    pi_error_few_args();

  if (!cdr(numbers)) {
    // (- number) => we have to invert the result
    if (!is_cons(numbers))
      pi_error(LISP_ERROR, "impossible to perform subtraction");
    if (!is_num(car(numbers)))
      pi_error(LISP_ERROR, "changing the number of a non-number");
    return mk_num(-(car(numbers)->value));
  } else {
    if (!is_cons(numbers) || !is_cons(cdr(numbers)))
      pi_error(LISP_ERROR, "impossible to perform subtraction");
    if (!is_num(car(numbers)) || !is_num(car(cdr(numbers))))
      pi_error(LISP_ERROR, "subtracted a non-number");
    long result = car(numbers)->value;
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
}

cell *multiplication(const cell *numbers) {
  long result = 1;
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
    pi_error_few_args();

  if (!is_cons(numbers) || !is_cons(cdr(numbers)))
    pi_error(LISP_ERROR, "impossible to perform division");
  if (!is_num(car(numbers)) || !is_num(car(cdr(numbers))))
    pi_error(LISP_ERROR, "divided a non-number");
  double result = (double)car(numbers)->value;
  const cell *act = cdr(numbers);
  while (act) {
    if (!is_cons(act))
      pi_error(LISP_ERROR, "impossible to perform division");
    if (!is_num(car(act)))
      pi_error(LISP_ERROR, "divided a non-number");
    if (car(act)->value == 0)
      pi_error(LISP_ERROR, "division by 0");
    result /= (double)car(act)->value;
    act = cdr(act);
  }
  return mk_num(result);
}

cell *car(const cell *c) {
  if (c == NULL)
    // (car NIL)
    return NULL;
  if (atom(c))
    pi_error(LISP_ERROR, "car applied to an atom");
  return c->car;
}
cell *cdr(const cell *c) {
  if (c == NULL)
    // (cdr NIL)
    return NULL;
  if (atom(c))
    pi_error(LISP_ERROR, "cdr applied to an atom");
  return c->cdr;
}
cell *caar(const cell *c) { return car(car(c)); }
cell *cddr(const cell *c) { return cdr(cdr(c)); }
cell *cadr(const cell *c) { return car(cdr(c)); }
cell *cdar(const cell *c) { return cdr(car(c)); }
cell *cadar(const cell *c) { return car(cdr(car(c))); }
cell *caddr(const cell *c) { return car(cdr(cdr(c))); }
cell *cons(cell *car, cell *cdr) { return mk_cons(car, cdr); }

cell *set(cell *name, cell *val, cell **env) {
  if (!is_sym(name))
    pi_error(LISP_ERROR, "first arg must be a symbol");
  cell *prec = NULL;
  cell *act = *env;
  while (act) {
    if (eq(name, caar(act))) {
      // found
      car(act)->cdr = val;
      return cdar(act);
    }
    // iterate
    prec = act;
    act = cdr(act);
  }
  cell *new = cons(cons(name, val), NULL);
  if (prec)
    prec->cdr = new;
  else
    (*env) = new;
  return val;
}

cell *load(cell *name, cell **env) {
  if (!name || !is_str(name))
    pi_error(LISP_ERROR, "first arg must me a string");
  FILE *file = fopen(((name) ? name->str : ""), "r");
  if (!file)
    pi_error(LISP_ERROR, "can't find file");
  while (!feof(file)) {
    cell *sexpr = read_sexpr(file);
    if (sexpr != symbol_file_ended)
      eval(sexpr, env);
  }
  return symbol_true;
}

cell *timer(cell *to_execute, cell **env) {
  clock_t t1, t2;
  long elapsed;

  t1 = clock();
  cell *valued = eval(to_execute, env);
  t2 = clock();

  elapsed = ((double)t2 - t1) / CLOCKS_PER_SEC * 1000;
  printf("time: %ld ms\n", elapsed);
  return valued;
}

cell * or (const cell *operands) {
  const cell *act = operands;
  cell *atom = car(act);
  while (act) {
    if (atom)
      return atom;
    act = cdr(act);
    atom = car(act);
  }
  return NULL;
}

cell * and (const cell *operands) {
  const cell *act = operands;
  const cell *prev = NULL;
  cell *atom = car(act);
  while (act) {
    if (!atom)
      // NIL found
      return atom;
    prev = act;
    act = cdr(act);
    atom = car(act);
  }
  if (!prev)
    return symbol_true;
  return car(prev);
}
cell * not(const cell *operands) {
  if (!operands)
    pi_error_few_args();
  if (cdr(operands))
    pi_error_many_args();
  if (car(operands))
    return NULL;
  else
    return symbol_true;
}

cell *greater(const cell *operands) {
  check_two_args(operands);
  const cell *first = car(operands);
  const cell *second = cadr(operands);
  if (first->type != second->type)
    pi_error(LISP_ERROR, "incompatible types");
  if (is_num(first)) {
    return ((first->value > second->value) ? symbol_true : NULL);
  } else if (is_str(first)) {
    return ((strcmp(first->str, second->str) > 0) ? symbol_true : NULL);
  } else
    pi_error(LISP_ERROR, "non-comparable args");
  return NULL;
}

cell *greater_eq(const cell *operands) {
  check_two_args(operands);
  const cell *first = car(operands);
  const cell *second = cadr(operands);
  if (first->type != second->type)
    pi_error(LISP_ERROR, "incompatible types");
  if (is_num(first)) {
    return ((first->value >= second->value) ? symbol_true : NULL);
  } else if (is_str(first)) {
    return ((strcmp(first->str, second->str) >= 0) ? symbol_true : NULL);
  } else
    pi_error(LISP_ERROR, "non-comparable args");
  return NULL;
}

cell *less(const cell *operands) {
  check_two_args(operands);
  const cell *first = car(operands);
  const cell *second = cadr(operands);
  if (first->type != second->type)
    pi_error(LISP_ERROR, "incompatible types");
  if (is_num(first)) {
    return ((first->value < second->value) ? symbol_true : NULL);
  } else if (is_str(first)) {
    return ((strcmp(first->str, second->str) < 0) ? symbol_true : NULL);
  } else
    pi_error(LISP_ERROR, "non-comparable args");
  return NULL;
}

cell *less_eq(const cell *operands) {
  check_two_args(operands);
  const cell *first = car(operands);
  const cell *second = cadr(operands);
  if (first->type != second->type)
    pi_error(LISP_ERROR, "incompatible types");
  if (is_num(first)) {
    return ((first->value <= second->value) ? symbol_true : NULL);
  } else if (is_str(first)) {
    return ((strcmp(first->str, second->str) <= 0) ? symbol_true : NULL);
  } else
    pi_error(LISP_ERROR, "non-comparable args");
  return NULL;
}

cell *length(const cell *list) {
  check_one_arg(list);
  if (!is_cons(list))
    pi_error(LISP_ERROR, "arg is not a list");
  unsigned long len = 0;
  const cell *act = car(list);
  if (act && !is_cons(act))
    pi_error(LISP_ERROR, "arg is not a list");
  while (act) {
    len++;
    act = cdr(act);
  }
  return mk_num(len);
}

cell *member(const cell *list) {
  check_two_args(
      list); // the first is the member and che second is the true list
  const cell *who = car(list);
  const cell *l = cadr(list);
  if (l && !is_cons(l))
    pi_error(LISP_ERROR, "second arg must be a list");
  cell *res = NULL;
  cell *head = NULL;
  bool found = false;
  while (l) {
    const cell *value = car(l);
    if (!found) {
      if (eq(value, who)) {
        // found
        found = true;
        res = mk_cons(copy_cell(value), NULL);
        head = res;
      }
    } else {
      // already found => we have to append this cell to the result
      cell *tmp = mk_cons(copy_cell(value), NULL);
      res->cdr = tmp;
      res = res->cdr;
    }
    l = cdr(l);
  }
  return head;
}

cell *nth(const cell *list) {
  check_two_args(list);
  const cell *num = car(list);
  if (!is_num(num))
    pi_error(LISP_ERROR, "first arg must be a number");
  const cell *l = cadr(list);
  if (l && !is_cons(l))
    pi_error(LISP_ERROR, "second arg must be a list");

  cell *res = NULL;
  unsigned long index = num->value;
  unsigned long act = 0;
  while (l && act < index) {
    l = cdr(l);
    act++;
  }
  if (l)
    res = car(l);
  return res;
}

bool total_eq(const cell *c1, const cell *c2) {
  if (!c1 && !c2)
    // NILL NILL
    return true;
  if (!c1 && c2)
    // NILL something
    return false;
  if (c1 && !c2)
    // something NILL
    return false;
  // something something
  if ((atom(c1) && !atom(c2)) || (!atom(c1) && atom(c2)))
    // one is an atom and the other is a cons
    return false;
  if (atom(c1) && atom(c2))
    // equality between two atoms
    return eq(c1, c2);
  // cons cons
  return total_eq(car(c1), car(c2)) && total_eq(cdr(c1), cdr(c2));
}