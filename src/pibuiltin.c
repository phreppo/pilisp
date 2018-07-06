#include "pibuiltin.h"
#include "pierror.h"

cell *addition(const cell *numbers) {
  long result = 0;
  const cell *act = numbers;
  while (act) {
    if (!is_cons(act))
      pi_error(LISP_ERROR, "impossible to perform addition");
    if (!is_num(car(act)))
      pi_error(LISP_ERROR, "added a non-number");
    result += car(act)->value;
    cell *tmp = cdr(act);
    cell_remove(car(act), SINGLE); // num used: we don't need it anymore
    cell_remove(act, SINGLE);
    act = tmp;
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
    int ret = -(car(numbers)->value);
    cell_remove(car(numbers), SINGLE);
    cell_remove(numbers, SINGLE);
    return mk_num(ret);
  } else {
    if (!is_cons(numbers) || !is_cons(cdr(numbers)))
      pi_error(LISP_ERROR, "impossible to perform subtraction");
    if (!is_num(car(numbers)) || !is_num(car(cdr(numbers))))
      pi_error(LISP_ERROR, "subtracted a non-number");
    long result = car(numbers)->value;
    const cell *act = cdr(numbers);
    cell_remove(car(numbers), SINGLE); // num used: we don't need it anymore
    cell_remove(numbers, SINGLE);
    while (act) {
      if (!is_cons(act))
        pi_error(LISP_ERROR, "impossible to perform subtraction");
      if (!is_num(car(act)))
        pi_error(LISP_ERROR, "subtracted a non-number");
      result -= car(act)->value;
      cell *tmp = cdr(act);
      cell_remove(car(act), SINGLE); // num used: we don't need it anymore
      cell_remove(act, SINGLE);
      act = tmp;
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

    cell *tmp = cdr(act);
    cell_remove(car(act), SINGLE); // num used: we don't need it anymore
    cell_remove(act, SINGLE);
    act = tmp;
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
  cell_remove(car(numbers), SINGLE); // num used: we don't need it anymore
  cell_remove(numbers, SINGLE);
  while (act) {
    if (!is_cons(act))
      pi_error(LISP_ERROR, "impossible to perform division");
    if (!is_num(car(act)))
      pi_error(LISP_ERROR, "divided a non-number");
    if (car(act)->value == 0)
      pi_error(LISP_ERROR, "division by 0");
    result /= (double)car(act)->value;

    cell *tmp = cdr(act);
    cell_remove(car(act), SINGLE); // num used: we don't need it anymore
    cell_remove(act, SINGLE);
    act = tmp;
  }
  return mk_num(result);
}


cell *set(cell *args) {
  check_two_args(args);
  cell *name = car(args);
  cell *val = cadr(args);
  if (!is_sym(name))
    pi_error(LISP_ERROR, "first arg must be a symbol");
  cell *prec = NULL;
  cell *act = memory->global_env;
  while (act) {
    if (eq(name, caar(act))) {
      // found
      car(act)->cdr = val;
      cell_remove_args(args);
      cell_remove(name, SINGLE);
      return cdar(act);
    }
    // iterate
    prec = act;
    act = cdr(act);
  }
  cell *pair = cons(name, val);
  cell *new = cons(pair, NULL);
  if (prec)
    prec->cdr = new;
  else
    memory->global_env = new;
  cell_remove(name, SINGLE);
  cell_remove(new, SINGLE);
  cell_remove(pair, SINGLE);
  cell_remove_args(args);
  return val;
}

cell *bye(cell *arg) { return symbol_bye; }

cell *mem_dump(cell *arg) {
  if (arg)
    pi_error_many_args();
  printf(ANSI_COLOR_YELLOW "============================== MEMORY "
                           "==============================\n" ANSI_COLOR_RESET);
  print_cell_space(memory);
  return symbol_true;
}

cell *timer(cell *arg, cell *env) {
  check_one_arg(arg);
  cell *to_execute = car(arg);
  clock_t t1, t2;
  long elapsed;

  t1 = clock();
  cell *valued = eval(to_execute, env);
  t2 = clock();

  elapsed = ((double)t2 - t1) / CLOCKS_PER_SEC * 1000;
  printf("time: %ld ms\n", elapsed);
  cell_remove_args(arg);
  return valued;
}

cell *quote(const cell *args, cell *env) {
  cell *evaulated = car(args);
  cell_remove(args, SINGLE);
  return evaulated;
}

cell *cond(const cell *arg, cell *env) { return evcon(arg, env); }

cell *write(cell *arg) {
  check_one_arg(arg);
  cell *target = car(arg);
  printf(ANSI_COLOR_GRAY " > " ANSI_COLOR_RESET);
  print_sexpr(target);
  puts("");
  cell_remove_args(arg);
  return target;
}

// ==================== LOGIC ====================

cell * or (const cell *operands) {
  const cell *act = operands;
  cell *atom = car(act);
  cell *tmp;
  while (act) {
    if (atom) {
      cell_remove(act, SINGLE);
      cell_remove(cdr(act), RECURSIVE);
      return atom;
    }

    tmp = cdr(act);
    cell_remove(act, SINGLE);
    cell_remove(car(act), SINGLE);
    act = tmp;
    atom = car(act);
  }
  return NULL;
}

cell * and (const cell *operands) {
  const cell *act = operands;
  const cell *prev = NULL;
  cell *atom = car(act);
  cell *tmp;
  while (act) {
    if (!atom) {
      // NIL found
      cell_remove(car(prev), RECURSIVE); // release the prev memory
      cell_remove(cdr(act), RECURSIVE);  // release the rest of the list
      cell_remove(act, SINGLE);          // release the act cons
      return NULL;
    }
    cell_remove(car(prev), RECURSIVE);
    prev = act;
    cell_push(car(prev), RECURSIVE); // protect the last value
    tmp = cdr(act);
    cell_remove(act, SINGLE);
    cell_remove(car(act), RECURSIVE);
    act = tmp;
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
  if (car(operands)) {
    cell_remove(operands, RECURSIVE);
    return NULL;
  } else {
    cell_remove(operands, SINGLE);
    return symbol_true;
  }
}

// ==================== COMPARISON ====================

cell *greater(const cell *operands) {
  check_two_args(operands);
  const cell *first = car(operands);
  const cell *second = cadr(operands);
  if (!first || !second)
    pi_lisp_error("NIL not allowed as arg");
  if (first->type != second->type)
    pi_error(LISP_ERROR, "incompatible types");
  cell *res = NULL;
  if (is_num(first)) {
    res = ((first->value > second->value) ? symbol_true : NULL);
  } else if (is_str(first)) {
    res = ((strcmp(first->str, second->str) > 0) ? symbol_true : NULL);
  } else
    pi_error(LISP_ERROR, "non-comparable args");
  cell_remove(operands, RECURSIVE);
  return res;
}

cell *greater_eq(const cell *operands) {
  check_two_args(operands);
  const cell *first = car(operands);
  const cell *second = cadr(operands);
  if (!first || !second)
    pi_lisp_error("NIL not allowed as arg");
  if (first->type != second->type)
    pi_error(LISP_ERROR, "incompatible types");
  cell *res = NULL;
  if (is_num(first)) {
    res = ((first->value >= second->value) ? symbol_true : NULL);
  } else if (is_str(first)) {
    res = ((strcmp(first->str, second->str) >= 0) ? symbol_true : NULL);
  } else
    pi_error(LISP_ERROR, "non-comparable args");
  cell_remove(operands, RECURSIVE);
  return res;
}

cell *less(const cell *operands) {
  check_two_args(operands);
  const cell *first = car(operands);
  const cell *second = cadr(operands);
  if (!first || !second)
    pi_lisp_error("NIL not allowed as arg");
  if (first->type != second->type)
    pi_error(LISP_ERROR, "incompatible types");
  cell *res = NULL;
  if (is_num(first)) {
    res = ((first->value < second->value) ? symbol_true : NULL);
  } else if (is_str(first)) {
    res = ((strcmp(first->str, second->str) < 0) ? symbol_true : NULL);
  } else
    pi_error(LISP_ERROR, "non-comparable args");
  cell_remove(operands, RECURSIVE);
  return res;
}

cell *less_eq(const cell *operands) {
  check_two_args(operands);
  const cell *first = car(operands);
  const cell *second = cadr(operands);
  if (!first || !second)
    pi_lisp_error("NIL not allowed as arg");
  if (first->type != second->type)
    pi_error(LISP_ERROR, "incompatible types");
  cell *res = NULL;
  if (is_num(first)) {
    res = ((first->value <= second->value) ? symbol_true : NULL);
  } else if (is_str(first)) {
    res = ((strcmp(first->str, second->str) <= 0) ? symbol_true : NULL);
  } else
    pi_error(LISP_ERROR, "non-comparable args");
  cell_remove(operands, RECURSIVE);
  return res;
}

// ==================== LISTS ====================

cell *length(const cell *list) {
  check_one_arg(list);
  unsigned long len = 0;
  const cell *act = car(list);
  if (act && !is_cons(act) && !is_str(act))
    pi_error(LISP_ERROR, "arg is not a list or a string");
  /********************************************************************************
   *                                  LEAKS MEMORY
   ********************************************************************************/
  if (act && is_str(act)) {
    cell_remove(list, SINGLE); // cons of the argument
    cell_remove(act, SINGLE);
    return mk_num(strlen(act->str));
  }
  cell *tmp;
  while (act) {
    len++;
    tmp = cdr(act);
    cell_remove(car(act), RECURSIVE); // remove sublist
    cell_remove(act, SINGLE);         // remove cons of the sublis
    act = tmp;
  }
  cell_remove(list, SINGLE); // cons of the argument
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
  cell *value;     // actual value
  cell *tmp;       // tmp to switch cell
  cell *next_copy; // start to copy
  while (l) {
    value = car(l);
    if (!found) {
      if (eq(value, who)) { // it's ok not total_eq here
        // found
        found = true;
        res = mk_cons(copy_cell(value), NULL);
        head = res;
      }
    } else {
      // already found => we have to append this cell to the result
      next_copy = mk_cons(copy_cell(value), NULL);
      res->cdr = next_copy;
      res = res->cdr;
    }
    tmp = cdr(l);
    cell_remove(l, SINGLE);
    cell_remove(car(l), RECURSIVE);
    l = tmp;
  }
  cell_remove(who, RECURSIVE);
  cell_remove_args(list); // cons of the two args

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
  cell *tmp;
  while (l && act < index) {
    tmp = cdr(l);
    cell_remove(car(l), RECURSIVE);
    cell_remove(l, SINGLE);
    l = tmp;
    act++;
  }
  if (l) { // cell is in the range: we can return it and free the rest of the
           // list
    res = car(l);
    cell_remove(l, SINGLE);         // remove the cons of the result
    cell_remove(cdr(l), RECURSIVE); // remove the rest of the list
  }
  cell_remove(num, SINGLE);
  cell_remove_args(list);
  return res;
}

cell *list(const cell *list) {
  cell *tmp = copy_cell(list);
  cell_remove(list, RECURSIVE);
  return tmp;
}

// ==================== BASIC APPLY ====================

cell *builtin_car(const cell *args) {
  check_one_arg(args);
  cell *res = caar(args);
  cell_remove(car(args), SINGLE);
  cell_remove(cdar(args), RECURSIVE); // remove the rest of the arg
  cell_remove_args(args);
  return res;
}
cell *builtin_cdr(const cell *args) {
  check_one_arg(args);
  cell *res = cdar(args);
  cell_remove(car(args), SINGLE);
  cell_remove(caar(args), RECURSIVE); // remove the car of the lists
  cell_remove_args(args);
  return res;
}
cell *builtin_cons(const cell *args) {
  check_two_args(args);
  cell *res = cons(car(args), cadr(args));
  cell_remove_args(args);
  return res;
}

cell *builtin_atom(const cell *args) {
  check_one_arg(args);
  cell *res;
  if (atom(car(args)))
    res = symbol_true;
  else
    res = NULL;
  cell_remove(args, RECURSIVE);
  return res;
}
cell *builtin_eq(const cell *args) {
  check_two_args(args);
  cell *res;
  if (eq(car(args), cadr(args)))
    res = symbol_true;
  else
    res = NULL;
  cell_remove(args, RECURSIVE);
  return res;
}
// ==================== MACROS ====================
cell *setq(const cell *args, cell *env) {
  check_two_args(args);
  cell *sym = car(args);
  if (!is_sym(sym))
    pi_lisp_error("first arg must be a symbol");
  cell *val = eval(cadr(args), env);
  cell *ret = set(mk_cons(sym, mk_cons(val, NULL)));
  cell_remove_args(args);
  return ret;
}

cell *let(const cell *args, cell *env) {
  cell *params = car(args);
  cell *body = cadr(args); // ok
  cell *new_env = env;

  cell *val;
  cell *new_pair;
  cell *tmp;

  while (params) {
    val = eval(cadar(params), env);        // give a value to val
    new_pair = mk_cons(caar(params), val); // (sym . val)
    new_env = mk_cons(new_pair,
                      new_env); // add on the head of the new env the new pair
    tmp = cdr(params);
    cell_remove(cdr(cdar(params)), SINGLE);
    cell_remove(cdar(params), SINGLE);
    cell_remove(car(params), SINGLE);
    cell_remove(params, SINGLE);
    params = tmp;
  }
  cell *res = eval(body, new_env);
  cell_remove_pairlis_deep(new_env, env);
  cell_remove_args(args);
  return res;
}

cell *defun(const cell *args, cell *env) {
  cell *fun_name = car(args);
  cell *lambda_struct = (cdr(args));
  cell *lambda_head = mk_cons(symbol_lambda, lambda_struct);
  cell *compacted = mk_cons(fun_name, mk_cons(lambda_head, NULL));
  set(compacted);
  cell_remove(args, SINGLE);
  return lambda_head;
}

cell *map(const cell *args, cell *env) {
  cell *func = car(args);
  cell *list = cadr(args);
  list = eval(list, env); // extract quote
  cell *result = NULL;
  cell *last_added = NULL;
  cell *val;
  cell *element;
  cell *tmp;
  while (list) {
    element = car(list);
    cell_push(func, RECURSIVE); // protect the function
    val = apply(func, mk_cons(eval(element, env), NULL), env, false);
    if (!result) {
      // we're creating the head
      result = last_added = mk_cons(val, NULL);
    } else {
      // we have at least one element => last_added is a node
      last_added->cdr = mk_cons(val, NULL);
      last_added = last_added->cdr;
    }
    tmp = cdr(list);
    cell_remove(list, SINGLE);
    list = tmp;
  }
  // (map 1+ '(1 2 3))
  cell_remove(func, RECURSIVE);
  cell_remove_args(args);
  return result;
}

cell *subseq(const cell *list) {
  cell *str = car(list);
  cell *start = cadr(list);
  size_t s = start->value;
  if (s > strlen(str->str))
    return NULL;
  if (cddr(list)) {
    puts("DUE ARGOMENTII");

    cell *end = caddr(list);
    size_t e = end->value;
    char *substr = malloc(e - s + 1);
    strncpy(substr, str->str + s, e - s);
    *(substr + (e - s)) = '\0';
    cell *ret = mk_str(substr);
    free(substr);
    cell_remove(list, RECURSIVE);
    return ret;
  } else {
    // just one number
    size_t e = strlen(str->str);
    char *substr = malloc(e - s + 1);
    strncpy(substr, str->str + s, e - s);
    *(substr + (e - s)) = '\0';
    cell *ret = mk_str(substr);
    cell_remove(list, RECURSIVE);
    return ret;
  }
}

cell *reverse(const cell *list) {
  check_one_arg(list);
  cell *act = car(list);
  cell *val;
  cell *tmp;
  cell *res = NULL;
  while (act) {
    tmp = cdr(act);
    val = car(act);
    res = mk_cons(val, res);
    cell_remove(act, SINGLE);
    act = tmp;
    // (reverse '(1 2 3))
  }
  cell_remove_args(list);
  return res;
}

cell *env(cell *arg) {
  if (arg)
    pi_error_many_args();
  printf(" > env: " ANSI_COLOR_BLUE);
  print_sexpr(memory->global_env);
  printf("\n" ANSI_COLOR_RESET);
  return symbol_true;
}

cell *integerp(const cell *arg) {
  bool ret = is_num(car(arg));
  cell_remove(car(arg), RECURSIVE);
  cell_remove(arg, SINGLE);
  return (ret ? symbol_true : NULL);
}
cell *symbolp(const cell *arg) {
  bool ret = is_sym(car(arg));
  cell_remove(car(arg), RECURSIVE);
  cell_remove(arg, SINGLE);
  return (ret ? symbol_true : NULL);
}

cell *collect_garbage_call(cell *arg) {
  collect_garbage(memory);
  return symbol_true;
}

cell *load(cell *arg, cell *env) {
  check_one_arg(arg);
  cell *name = eval(car(arg), env); // extract the name
  if (!name || !is_str(name))
    pi_error(LISP_ERROR, "first arg must me a string");
  FILE *file = fopen(((name) ? name->str : ""), "r");
  if (!file)
    pi_error(LISP_ERROR, "can't find file");
  cell *last_result;
  while (!feof(file)) {
    cell *sexpr = read_sexpr(file);
    if (sexpr != symbol_file_ended) {
      // eval only if you didn't read an empty fragment
      last_result = eval(sexpr, env);
      cell_remove(last_result, RECURSIVE);
    }
  }
  cell_remove(name, SINGLE);
  cell_remove_args(arg);
  return symbol_true;
}

cell *dotimes(const cell *arg, cell *env) {
  // DOTIMES
  size_t n = 0;
  cell *name_list = car(arg);
  cell *num = car(cdr(car(arg)));
  cell *expr = cadr(arg);
  cell *new_env;
  for (n = 0; n < num->value; n++) {
    cell *num_list_new = mk_cons(mk_num(n), NULL);
    new_env = pairlis(name_list, num_list_new, env);
    if (n > 0)
      // we have to protect the body of the function
      cell_push(expr, RECURSIVE);
    cell *evaulated = eval(expr, new_env);
    // remove the result
    cell_remove(evaulated, RECURSIVE);
    // remove the pair (n [actual_value])
    cell_remove_pairlis(new_env, env);
    // remove the just created cell
    cell_remove(num_list_new, RECURSIVE);
  }
  cell_remove(car(arg),
              RECURSIVE); // remove the pair and cons (n [number])
  return NULL;
}