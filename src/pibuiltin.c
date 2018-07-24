#include "pibuiltin.h"
#include "pierror.h"

cell *append(cell *list) {
#if CHECKS
  check_two_args(list);
#endif
  cell *first_list = car(list);
  cell *second_list = cadr(list);
#if CHECKS
  if (first_list && !is_cons(first_list))
    pi_lisp_error("first arg must be a list");
#endif
  cell *act = first_list;
  while (act && cdr(act)) {
    act = cdr(act);
  }
  if (act)
    act->cdr = second_list;
  else 
    first_list = second_list;
  cell_remove_args(list);
  return first_list;
}

cell * asm_call(cell *args, cell * env) {
  return asm_call_with_stack_base(args,env,stack_pointer);
}

// handles only strings
cell *concatenate(const cell *list) {
#if CHECKS
  check_three_args(list);
#endif
  cell *symbol_type = car(list);
  cell *first_string = cadr(list);
  cell *second_string = caddr(list);
#if CHECKS
  if (!is_sym(symbol_type))
    pi_lisp_error("first arg must be a symbol");
  if (!is_str(first_string))
    pi_lisp_error("second arg must be a string");
  if (!is_str(second_string))
    pi_lisp_error("third arg must be a string");
    // if (symbol_type != symbol_string)
    //   pi_lisp_error("you can concatenate only strings");
#endif
  char *first = first_string->str;
  char *second = second_string->str;
  char *new_str = malloc(sizeof(first) + sizeof(second) + 1);
  strcpy(new_str, first);
  strcat(new_str, second);
  unsafe_cell_remove(first_string);
  unsafe_cell_remove(second_string);
  // unsafe_cell_remove(symbol_type); // why don t remove? it a global symbol
  // that is not in one protected zone, so we can t unmark it
  cell_remove_args(list);
  return mk_str(new_str);
}

// (concatenate 'string "ciao " "sono pazzo")

cell *addition(const cell *numbers) {
  long result = 0;
  const cell *act = numbers;
  cell *tmp;
  while (act) {
#if CHECKS
    if (!is_cons(act))
      pi_error(LISP_ERROR, "impossible to perform addition");
    if (!is_num(car(act)))
      pi_error(LISP_ERROR, "added a non-number");
#endif
    result += car(act)->value;
    tmp = cdr(act);
    unsafe_cell_remove(car(act)); // num used: we don't need it anymore
    unsafe_cell_remove(act);
    act = tmp;
  }
  return mk_num(result);
}

cell *subtraction(const cell *numbers) {
#if CHECKS
  if (!numbers)
    // we need 1 argument at least
    pi_error_few_args();
#endif

  if (!cdr(numbers)) {
// (- number) => we have to invert the result
#if CHECKS
    if (!is_cons(numbers))
      pi_error(LISP_ERROR, "impossible to perform subtraction");
    if (!is_num(car(numbers)))
      pi_error(LISP_ERROR, "changing the number of a non-number");
#endif
    int ret = -(car(numbers)->value);
    unsafe_cell_remove(car(numbers));
    unsafe_cell_remove(numbers);
    return mk_num(ret);
  } else {
#if CHECKS
    if (!is_cons(numbers) || !is_cons(cdr(numbers)))
      pi_error(LISP_ERROR, "impossible to perform subtraction");
    if (!is_num(car(numbers)) || !is_num(car(cdr(numbers))))
      pi_error(LISP_ERROR, "subtracted a non-number");
#endif
    long result = car(numbers)->value;
    const cell *act = cdr(numbers);
    unsafe_cell_remove(car(numbers)); // num used: we don't need it anymore
    unsafe_cell_remove(numbers);
    cell *tmp;
    while (act) {
#if CHECKS
      if (!is_cons(act))
        pi_error(LISP_ERROR, "impossible to perform subtraction");
      if (!is_num(car(act)))
        pi_error(LISP_ERROR, "subtracted a non-number");
#endif
      result -= car(act)->value;
      tmp = cdr(act);
      unsafe_cell_remove(car(act)); // num used: we don't need it anymore
      unsafe_cell_remove(act);
      act = tmp;
    }
    return mk_num(result);
  }
}

cell *multiplication(const cell *numbers) {
  long result = 1;
  const cell *act = numbers;
  cell *tmp;
  while (act) {
#if CHECKS
    if (!is_cons(act))
      pi_error(LISP_ERROR, "impossible to perform multiplication");
    if (!is_num(car(act)))
      pi_error(LISP_ERROR, "multiplicated a non-number");
#endif
    result *= car(act)->value;

    tmp = cdr(act);
    unsafe_cell_remove(car(act)); // num used: we don't need it anymore
    unsafe_cell_remove(act);
    act = tmp;
  }
  return mk_num(result);
}

cell *division(const cell *numbers) {
#if CHECKS
  if (!numbers || !cdr(numbers))
    // we need 2 numbers at least
    pi_error_few_args();
  if (!is_cons(numbers) || !is_cons(cdr(numbers)))
    pi_error(LISP_ERROR, "impossible to perform division");
  if (!is_num(car(numbers)) || !is_num(car(cdr(numbers))))
    pi_error(LISP_ERROR, "divided a non-number");
#endif
  double result = (double)car(numbers)->value;
  const cell *act = cdr(numbers);
  unsafe_cell_remove(car(numbers)); // num used: we don't need it anymore
  unsafe_cell_remove(numbers);
  cell *tmp;
  while (act) {
#if CHECKS
    if (!is_cons(act))
      pi_error(LISP_ERROR, "impossible to perform division");
    if (!is_num(car(act)))
      pi_error(LISP_ERROR, "divided a non-number");
#endif
    if (car(act)->value == 0)
      pi_error(LISP_ERROR, "division by 0");
    result /= (double)car(act)->value;

    tmp = cdr(act);
    unsafe_cell_remove(car(act)); // num used: we don't need it anymore
    unsafe_cell_remove(act);
    act = tmp;
  }
  return mk_num(result);
}

cell *set(cell *args) {
#if CHECKS
  check_two_args(args);
#endif
  cell *name = car(args);
  cell *val = cadr(args);

  /*
  
  if (is_lambda(val)){
    val = compile(val);
  }

  cell * compile(val){
    scrivi in un file il programma: 
      "(plc '[val])"
    esegui il programma e ritorna il risultato
    ritorna quello che ha tornato il risultato
  }
  
  */
  
#if CHECKS
  if (!is_sym(name))
    pi_error(LISP_ERROR, "first arg must be a symbol");
#endif
  cell *prec = NULL;
  cell *act = memory->global_env;
  while (act) {
    if (eq(name, caar(act))) {
      // found
      cell_remove_recursive(car(act)->cdr); // remove old val
      car(act)->cdr = val;
      cell_push_recursive(val);
      unsafe_cell_remove(name);
      cell_remove_args(args);
      return cdar(act);
    }
    // iterate
    prec = act;
    act = cdr(act);
  }
  cell *pair = cons(name, val);
  cell_push_recursive(pair);
  cell *new = cons(pair, NULL);
  if (prec)
    prec->cdr = new;
  else
    memory->global_env = new;
  unsafe_cell_remove(name);
  unsafe_cell_remove(pair);
  cell_remove_args(args);
  return val;
}

cell *bye(cell *arg) { return symbol_bye; }

cell *mem_dump(cell *arg) {
#if CHECKS
  if (arg)
    pi_error_many_args();
#endif
  printf(ANSI_COLOR_YELLOW "============================== MEMORY "
                           "==============================\n" ANSI_COLOR_RESET);
  print_cell_space(memory);
  return symbol_true;
}

cell *timer(cell *arg, cell *env) {
#if CHECKS
  check_one_arg(arg);
#endif
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
  unsafe_cell_remove(args);
  return evaulated;
}

cell *cond(const cell *arg, cell *env) { return evcon(arg, env); }

cell *write(cell *arg) {
#if CHECKS
  check_one_arg(arg);
#endif
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
      // found not nil
      cell_remove_recursive(cdr(act));
      unsafe_cell_remove(act);
      return atom;
    }

    tmp = cdr(act);
    cell_remove(car(act));
    // cons
    unsafe_cell_remove(act);
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
      cell_remove_recursive(car(prev)); // release the prev memory
      cell_remove_recursive(cdr(act));  // release the rest of the list
      unsafe_cell_remove(act);          // release the act cons
      return NULL;
    }
    cell_remove_recursive(car(prev));
    prev = act;
    cell_push_recursive(car(prev)); // protect the last value
    tmp = cdr(act);
    cell_remove_recursive(car(act));
    unsafe_cell_remove(act);
    act = tmp;
    atom = car(act);
  }
  if (!prev)
    return symbol_true;
  return car(prev);
}

cell * not(const cell *operands) {
#if CHECKS
  if (!operands)
    pi_error_few_args();
  if (cdr(operands))
    pi_error_many_args();
#endif
  if (car(operands)) {
    cell_remove_recursive(operands);
    return NULL;
  } else {
    unsafe_cell_remove(operands);
    return symbol_true;
  }
}

// ==================== COMPARISON ====================

cell *greater(const cell *operands) {
#if CHECKS
  check_two_args(operands);
#endif
  const cell *first = car(operands);
  const cell *second = cadr(operands);
#if CHECKS
  if (!first || !second)
    pi_lisp_error("NIL not allowed as arg");
  if (first->type != second->type)
    pi_error(LISP_ERROR, "incompatible types");
#endif
  cell *res = NULL;
  if (is_num(first)) {
    res = ((first->value > second->value) ? symbol_true : NULL);
  } else if (is_str(first)) {
    res = ((strcmp(first->str, second->str) > 0) ? symbol_true : NULL);
  } else
    pi_error(LISP_ERROR, "non-comparable args");
  cell_remove_recursive(operands);
  return res;
}

cell *greater_eq(const cell *operands) {
#if CHECKS
  check_two_args(operands);
#endif
  const cell *first = car(operands);
  const cell *second = cadr(operands);
#if CHECKS
  if (!first || !second)
    pi_lisp_error("NIL not allowed as arg");
  if (first->type != second->type)
    pi_error(LISP_ERROR, "incompatible types");
#endif
  cell *res = NULL;
  if (is_num(first)) {
    res = ((first->value >= second->value) ? symbol_true : NULL);
  } else if (is_str(first)) {
    res = ((strcmp(first->str, second->str) >= 0) ? symbol_true : NULL);
  } else
    pi_error(LISP_ERROR, "non-comparable args");
  cell_remove_recursive(operands);
  return res;
}

cell *less(const cell *operands) {
#if CHECKS
  check_two_args(operands);
#endif
  const cell *first = car(operands);
  const cell *second = cadr(operands);
#if CHECKS
  if (!first || !second)
    pi_lisp_error("NIL not allowed as arg");
  if (first->type != second->type)
    pi_error(LISP_ERROR, "incompatible types");
#endif
  cell *res = NULL;
  if (is_num(first)) {
    res = ((first->value < second->value) ? symbol_true : NULL);
  } else if (is_str(first)) {
    res = ((strcmp(first->str, second->str) < 0) ? symbol_true : NULL);
  } else
    pi_error(LISP_ERROR, "non-comparable args");
  cell_remove_recursive(operands);
  return res;
}

cell *less_eq(const cell *operands) {
#if CHECKS
  check_two_args(operands);
#endif
  const cell *first = car(operands);
  const cell *second = cadr(operands);
#if CHECKS
  if (!first || !second)
    pi_lisp_error("NIL not allowed as arg");
  if (first->type != second->type)
    pi_error(LISP_ERROR, "incompatible types");
#endif
  cell *res = NULL;
  if (is_num(first)) {
    res = ((first->value <= second->value) ? symbol_true : NULL);
  } else if (is_str(first)) {
    res = ((strcmp(first->str, second->str) <= 0) ? symbol_true : NULL);
  } else
    pi_error(LISP_ERROR, "non-comparable args");
  cell_remove_recursive(operands);
  return res;
}

// ==================== LISTS ====================

cell *length(const cell *list) {
#if CHECKS
  check_one_arg(list);
#endif
  unsigned long len = 0;
  const cell *act = car(list);
#if CHECKS
  if (act && !is_cons(act) && !is_str(act))
    pi_error(LISP_ERROR, "arg is not a list or a string");
#endif
  /********************************************************************************
   *                                  LEAKS MEMORY
   ********************************************************************************/
  if (act && is_str(act)) {
    cell *ret = mk_num(strlen(act->str));
    unsafe_cell_remove(list); // cons of the argument
    unsafe_cell_remove(act);
    return ret;
  }
  cell *tmp;
  while (act) {
    len++;
    tmp = cdr(act);
    cell_remove_recursive(car(act)); // remove sublist
    unsafe_cell_remove(act);         // remove cons of the sublis
    act = tmp;
  }
  unsafe_cell_remove(list); // cons of the argument
  return mk_num(len);
}

cell *member(const cell *list) {
#if CHECKS
  check_two_args(
      list); // the first is the member and che second is the true list
#endif
  const cell *who = car(list);
  const cell *l = cadr(list);
#if CHECKS
  if (l && !is_cons(l))
    pi_error(LISP_ERROR, "second arg must be a list");
#endif
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
    cell_remove_recursive(car(l));
    unsafe_cell_remove(l);
    l = tmp;
  }
  cell_remove_recursive(who);
  cell_remove_args(list); // cons of the two args

  return head;
}

cell *nth(const cell *list) {
#if CHECKS
  check_two_args(list);
#endif
  const cell *num = car(list);
#if CHECKS
  if (!is_num(num))
    pi_error(LISP_ERROR, "first arg must be a number");
#endif
  const cell *l = cadr(list);
#if CHECKS
  if (l && !is_cons(l))
    pi_error(LISP_ERROR, "second arg must be a list");
#endif

  cell *res = NULL;
  unsigned long index = num->value;
  unsigned long act = 0;
  cell *tmp;
  while (l && act < index) {
    tmp = cdr(l);
    cell_remove_recursive(car(l));
    unsafe_cell_remove(l);
    l = tmp;
    act++;
  }
  if (l) { // cell is in the range: we can return it and free the rest of the
           // list
    res = car(l);
    cell_remove_recursive(cdr(l)); // remove the rest of the list
    unsafe_cell_remove(l);         // remove the cons of the result
  }
  unsafe_cell_remove(num);
  cell_remove_args(list);
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

cell *list(const cell *list) {
  cell *tmp = copy_cell(list);
  cell_remove_recursive(list);
  return tmp;
}

// ==================== BASIC APPLY ====================

cell *builtin_car(const cell *args) {
#if CHECKS
  check_one_arg(args);
#endif
  cell *res = caar(args);
  cell_remove_recursive(cdar(args)); // remove the rest of the arg
  cell_remove(car(args));
  cell_remove_args(args);
  return res;
}
cell *builtin_cdr(const cell *args) {
#if CHECKS
  check_one_arg(args);
#endif
  cell *res = cdar(args);
  cell_remove_recursive(caar(args)); // remove the car of the lists
  cell_remove(car(args));
  cell_remove_args(args);
  return res;
}
cell *builtin_cons(const cell *args) {
#if CHECKS
  check_two_args(args);
#endif
  cell *res = cons(car(args), cadr(args));
  cell_remove_args(args);
  return res;
}

cell *builtin_atom(const cell *args) {
#if CHECKS
  check_one_arg(args);
#endif
  cell *res;
  if (atom(car(args)))
    res = symbol_true;
  else
    res = NULL;
  cell_remove_recursive(args);
  return res;
}
cell *builtin_eq(const cell *args) {
#if CHECKS
  check_two_args(args);
#endif
  cell *res;
  if (eq(car(args), cadr(args)))
    res = symbol_true;
  else
    res = NULL;
  cell_remove_recursive(args);
  return res;
}
// ==================== MACROS ====================
cell *setq(const cell *args, cell *env) {
#if CHECKS
  check_two_args(args);
#endif
  cell *sym = car(args);
#if CHECKS
  if (!is_sym(sym))
    pi_lisp_error("first arg must be a symbol");
#endif
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
    cell_remove(cdr(cdar(params)));
    cell_remove_recursive(cdar(params)); // maybe null
    unsafe_cell_remove(car(params));     // cons
    unsafe_cell_remove(params);
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
  cell_remove(args);
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
    cell_push_recursive(func); // protect the function
    element = car(list);
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
    unsafe_cell_remove(list);
    list = tmp;
  }
  // (map 1+ '(1 2 3))
  cell_remove_recursive(func);
  cell_remove_args(args);
  return result;
}

cell *subseq(const cell *list) {
  cell *str = car(list);
  cell *start = cadr(list);
  size_t s = start->value;
  if (s > strlen(str->str)) {
    cell_remove_recursive(list);
    return NULL;
  }
  if (cddr(list)) {
    cell *end = caddr(list);
    size_t e = end->value;
    char *substr = malloc(e - s + 1);
    strncpy(substr, str->str + s, e - s);
    *(substr + (e - s)) = '\0';
    cell *ret = mk_str(substr);
    free(substr);
    cell_remove_recursive(list);
    return ret;
  } else {
    // just one number
    size_t e = strlen(str->str);
    char *substr = malloc(e - s + 1);
    strncpy(substr, str->str + s, e - s);
    *(substr + (e - s)) = '\0';
    cell *ret = mk_str(substr);
    cell_remove_recursive(list);
    return ret;
  }
}

cell *reverse(const cell *list) {
#if CHECKS
  check_one_arg(list);
#endif
  cell *act = car(list);
  cell *val;
  cell *tmp;
  cell *res = NULL;
  while (act) {
    tmp = cdr(act);
    val = car(act);
    res = mk_cons(val, res);
    unsafe_cell_remove(act);
    act = tmp;
    // (reverse '(1 2 3))
  }
  cell_remove_args(list);
  return res;
}

cell *env(cell *arg) {
#if CHECKS
  if (arg)
    pi_error_many_args();
#endif
  printf(" > env: " ANSI_COLOR_BLUE);
  print_sexpr(memory->global_env);
  printf("\n" ANSI_COLOR_RESET);
  return symbol_true;
}

cell *integerp(const cell *arg) {
  bool ret = is_num(car(arg));
  cell_remove_recursive(car(arg));
  unsafe_cell_remove(arg);
  return (ret ? symbol_true : NULL);
}
cell *symbolp(const cell *arg) {
  bool ret = is_sym(car(arg));
  cell_remove_recursive(car(arg));
  unsafe_cell_remove(arg);
  return (ret ? symbol_true : NULL);
}

cell *collect_garbage_call(cell *arg) {
  deep_collect_garbage(memory);
  return symbol_true;
}

cell *load(cell *arg, cell *env) {
#if CHECKS
  check_one_arg(arg);
#endif
  cell *name = eval(car(arg), env); // extract the name
#if CHECKS
  if (!name || !is_str(name))
    pi_error(LISP_ERROR, "first arg must me a string");
#endif
  FILE *file = fopen(((name) ? name->str : ""), "r");
  if (!file)
    pi_error(LISP_ERROR, "can't find file");
  cell *last_result;
  while (!feof(file)) {
    cell *sexpr = read_sexpr(file);
    if (sexpr != symbol_file_ended) {
      // eval only if you didn't read an empty fragment
      last_result = eval(sexpr, env);
      cell_remove_recursive(last_result);
    }
  }
  unsafe_cell_remove(name);
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
    if (n > 0) {
      // we have to protect the body of the function
      cell_push_recursive(expr);
      // cell_push(caar(arg)); // name of the parameter (n)
    }
    cell *actual_n_value = mk_num(n);
    cell *num_list_new = mk_cons(actual_n_value, NULL);
    new_env = pairlis(name_list, num_list_new, env);
    cell *evaulated = eval(expr, new_env);
    // remove the result
    cell_remove_recursive(evaulated);
    // remove the pair (n [actual_value])
    cell_remove_pairlis(new_env, env);
    // remove the just created cell
    cell_remove_recursive(num_list_new);
    // REMOVE THE ACTUAL VALUE OF N
    // unsafe_cell_remove(actual_n_value);
  }
  // cons of the pair
  unsafe_cell_remove(cdr(arg));
  cell_remove(arg);
  cell_remove_recursive(car(arg)); // remove the pair and cons (n [number])
  return NULL;
}
#if !INLINE_FUNCTIONS
cell *caar(const cell *c) { return c->car->car; }
cell *cddr(const cell *c) { return c->cdr->cdr; }
cell *cadr(const cell *c) { return c->cdr->car; }
cell *cdar(const cell *c) { return c->car->cdr; }
cell *cadar(const cell *c) { return c->car->cdr->car; }
cell *caddr(const cell *c) { return c->cdr->cdr->car; }
cell *cons(cell *car, cell *cdr) { return mk_cons(car, cdr); }

int atom(const cell *c) {
  return (c == NULL) ||
         (c->type == TYPE_SYM || c->type == TYPE_NUM || c->type == TYPE_STR ||
          c->type == TYPE_BUILTINLAMBDA || c->type == TYPE_BUILTINMACRO ||
          c->type == TYPE_KEYWORD);
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

cell *car(const cell *c) {
  if (c == NULL)
    return NULL;
#if CHECKS
  if (atom(c))
    pi_lisp_error("car applied to an atom");
#endif
  return c->car;
}

cell *cdr(const cell *c) {
  if (c == NULL)
    return NULL;
#if CHECKS
  if (atom(c))
    pi_lisp_error("cdr applied to an atom");
#endif
  return c->cdr;
}
#endif