#include "pibuiltin.h"
#include "pierror.h"
#include "piremove.h"

/********************************************************************************
 *                                Basic Apply
 ********************************************************************************/

cell *builtin_car(cell *args) {
#if CHECKS
  check_one_arg(args);
#endif
  cell *res = caar(args);

  cell_remove_recursive(cdar(args));
  cell_remove(car(args));
  cell_remove_args(args);

  return res;
}

cell *builtin_cdr(cell *args) {
#if CHECKS
  check_one_arg(args);
#endif
  cell *res = cdar(args);

  cell_remove_recursive(caar(args));
  cell_remove(car(args));
  cell_remove_args(args);

  return res;
}
cell *builtin_cons(cell *args) {
#if CHECKS
  check_two_args(args);
#endif
  cell *res = cons(car(args), cadr(args));
  cell_remove_args(args);
  return res;
}

cell *builtin_atom(cell *args) {
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

cell *builtin_eq(cell *args) {
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

/********************************************************************************
 *                                  Arithmetic
 ********************************************************************************/

cell *addition(cell *numbers) {
  long result = 0;
  cell *act = numbers;
  cell *tmp;

  while (act) {
#if CHECKS
    check_addition_atom(act);
#endif
    result += car(act)->value;
    tmp = cdr(act);

    unsafe_cell_remove(car(act));
    unsafe_cell_remove(act);
    act = tmp;
  }

  return mk_num(result);
}

cell *subtraction(cell *numbers) {
#if CHECKS
  check_subtraction(numbers);
#endif

  if (!cdr(numbers))
    return subtraction_invert_result(numbers);
  else
    return subtraction_two_or_more_numbers(numbers);
}

cell *subtraction_invert_result(cell *numbers) {
#if CHECKS
  check_subtraction_atom(numbers);
#endif
  int ret = -(car(numbers)->value);
  unsafe_cell_remove(car(numbers));
  unsafe_cell_remove(numbers);

  return mk_num(ret);
}

cell *subtraction_two_or_more_numbers(cell *numbers) {
#if CHECKS
  check_subtraction_atom(numbers);
#endif
  long result = car(numbers)->value;
  cell *act = cdr(numbers);
  cell *tmp;

  unsafe_cell_remove(car(numbers)); // num used: we don't need it anymore
  unsafe_cell_remove(numbers);

  while (act) {
#if CHECKS
    check_subtraction_atom(act);
#endif
    result -= car(act)->value;
    tmp = cdr(act);
    unsafe_cell_remove(car(act));
    unsafe_cell_remove(act);
    act = tmp;
  }

  return mk_num(result);
}

cell *multiplication(cell *numbers) {
  long result = 1;
  cell *act = numbers;
  cell *tmp;

  while (act) {
#if CHECKS
    check_multiplication_atom(act);
#endif
    result *= car(act)->value;
    tmp = cdr(act);
    unsafe_cell_remove(car(act));
    unsafe_cell_remove(act);
    act = tmp;
  }

  return mk_num(result);
}

cell *division(cell *numbers) {
#if CHECKS
  check_division(numbers);
#endif
  double result = (double)car(numbers)->value;
  cell *act = cdr(numbers);
  cell *tmp;

  unsafe_cell_remove(car(numbers));
  unsafe_cell_remove(numbers);

  while (act) {
#if CHECKS
    check_division_atom(act);
#endif
    result /= (double)car(act)->value;

    tmp = cdr(act);
    unsafe_cell_remove(car(act));
    unsafe_cell_remove(act);
    act = tmp;
  }

  return mk_num(result);
}

/********************************************************************************
 *                                     Logic
 ********************************************************************************/

cell * or (cell * operands) {
  cell *act = operands;
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
    unsafe_cell_remove(act);
    act = tmp;
    atom = car(act);
  }

  return NULL;
}

cell * and (cell * operands) {
  cell *act = operands;
  cell *prev = NULL;
  cell *atom = car(act);
  cell *tmp;

  while (act) {
    if (!atom) {
      cell_remove_recursive(car(prev));
      cell_remove_recursive(cdr(act));
      unsafe_cell_remove(act);
      return NULL;
    }

    prev = act;
    tmp = cdr(act);
    cell_remove_recursive(car(prev));
    cell_push_recursive(car(prev)); // protect the last value
    cell_remove_recursive(car(act));
    unsafe_cell_remove(act);
    act = tmp;
    atom = car(act);
  }

  if (!prev)
    return symbol_true;
  return car(prev);
}

cell * not(cell * operands) {
#if CHECKS
  check_one_arg(operands);
#endif
  if (car(operands)) {
    cell_remove_recursive(operands);
    return NULL;
  } else {
    unsafe_cell_remove(operands);
    return symbol_true;
  }
}

/********************************************************************************
 *                                  Comparison
 ********************************************************************************/

cell *greater(cell *operands) {
#if CHECKS
  check_comparables(operands);
#endif
  cell *first = car(operands);
  cell *second = cadr(operands);
  cell *res = NULL;

  if (is_num(first))
    res = compare_greater_numbers(first, second);
  else if (is_str(first))
    res = compare_greater_strings(first, second);
  else
    pi_lisp_error("non-comparable args");

  cell_remove_recursive(operands);
  return res;
}

cell *compare_greater_numbers(cell *first_num, cell *second_num) {
  return ((first_num->value > second_num->value) ? symbol_true : NULL);
}

cell *compare_greater_strings(cell *first_str, cell *second_str) {
  return ((strcmp(first_str->str, second_str->str) > 0) ? symbol_true : NULL);
}

cell *greater_eq(cell *operands) {
#if CHECKS
  check_comparables(operands);
#endif
  cell *first = car(operands);
  cell *second = cadr(operands);
  cell *res = NULL;

  if (is_num(first))
    res = compare_greater_eq_numbers(first, second);
  else if (is_str(first))
    res = compare_greater_eq_strings(first, second);
  else
    pi_lisp_error("non-comparable args");

  cell_remove_recursive(operands);
  return res;
}

cell *compare_greater_eq_numbers(cell *first_num, cell *second_num) {
  return ((first_num->value >= second_num->value) ? symbol_true : NULL);
}

cell *compare_greater_eq_strings(cell *first_str, cell *second_str) {
  return ((strcmp(first_str->str, second_str->str) >= 0) ? symbol_true : NULL);
}

cell *less(cell *operands) {
#if CHECKS
  check_comparables(operands);
#endif
  cell *first = car(operands);
  cell *second = cadr(operands);
  cell *res = NULL;

  if (is_num(first))
    res = compare_less_numbers(first, second);
  else if (is_str(first))
    res = compare_less_strings(first, second);
  else
    pi_lisp_error("non-comparable args");

  cell_remove_recursive(operands);
  return res;
}

cell *compare_less_numbers(cell *first_num, cell *second_num) {
  return ((first_num->value < second_num->value) ? symbol_true : NULL);
}
cell *compare_less_strings(cell *first_str, cell *second_str) {
  return ((strcmp(first_str->str, second_str->str) < 0) ? symbol_true : NULL);
}

cell *less_eq(cell *operands) {
#if CHECKS
  check_comparables(operands);
#endif
  cell *first = car(operands);
  cell *second = cadr(operands);
  cell *res = NULL;

  if (is_num(first))
    res = compare_less_eq_numbers(first, second);
  else if (is_str(first))
    res = compare_less_eq_strings(first, second);
  else
    pi_lisp_error("non-comparable args");

  cell_remove_recursive(operands);
  return res;
}

cell *compare_less_eq_numbers(cell *first_num, cell *second_num) {
  return ((first_num->value <= second_num->value) ? symbol_true : NULL);
}
cell *compare_less_eq_strings(cell *first_str, cell *second_str) {
  return ((strcmp(first_str->str, second_str->str) <= 0) ? symbol_true : NULL);
}

cell *integerp(cell *arg) {
#if CHECKS
  check_one_arg(arg);
#endif
  bool ret = is_num(car(arg));
  cell_remove_recursive(car(arg));
  unsafe_cell_remove(arg);

  return (ret ? symbol_true : NULL);
}
cell *symbolp(cell *arg) {
#if CHECKS
  check_one_arg(arg);
#endif
  bool ret = is_sym(car(arg));
  cell_remove_recursive(car(arg));
  unsafe_cell_remove(arg);

  return (ret ? symbol_true : NULL);
}

/********************************************************************************
 *                                   Lists
 ********************************************************************************/

cell *list(cell *args) { return args; }

cell *reverse(cell *args) {
#if CHECKS
  check_one_arg(args);
#endif
  cell *rest_of_the_list = car(args);
  cell *reversed_list = NULL;
  cell *actual_car;
  cell *tmp;

  while (rest_of_the_list) {
    actual_car = car(rest_of_the_list);
    reversed_list = mk_cons(actual_car, reversed_list);
    tmp = cdr(rest_of_the_list);
    unsafe_cell_remove(rest_of_the_list);
    rest_of_the_list = tmp;
  }

  cell_remove_args(args);
  return reversed_list;
}

cell *member(cell *args) {
#if CHECKS
  check_member(args);
#endif
  cell *wanted = car(args);
  cell *rest_of_the_list = cadr(args);
  cell *res = NULL;
  cell *head = NULL;
  bool found = false;
  cell *actual_member;
  cell *tmp;       // tmp to switch cell
  cell *next_copy; // start to copy

  while (rest_of_the_list) {
    actual_member = car(rest_of_the_list);
    if (!found) {
      if (eq(actual_member, wanted)) {
        found = true;
        res = mk_cons(copy_cell(actual_member), NULL);
        head = res;
      }
    } else {
      next_copy = mk_cons(copy_cell(actual_member), NULL);
      res->cdr = next_copy;
      res = res->cdr;
    }
    tmp = cdr(rest_of_the_list);
    cell_remove_recursive(car(rest_of_the_list));
    unsafe_cell_remove(rest_of_the_list);
    rest_of_the_list = tmp;
  }

  cell_remove_recursive(wanted);
  cell_remove_args(args);
  return head;
}

cell *nth(cell *args) {
#if CHECKS
  check_nth(args);
#endif
  cell *num = car(args);
  cell *rest_of_the_list = cadr(args);
  cell *res = NULL;
  unsigned long index = num->value;
  unsigned long act_index = 0;
  cell *tmp;

  while (rest_of_the_list && act_index < index) {
    tmp = cdr(rest_of_the_list);
    cell_remove_recursive(car(rest_of_the_list));
    unsafe_cell_remove(rest_of_the_list);
    rest_of_the_list = tmp;
    act_index++;
  }

  if (rest_of_the_list) {
    res = car(rest_of_the_list);
    cell_remove_recursive(cdr(rest_of_the_list)); // rest of the list
    unsafe_cell_remove(rest_of_the_list);         // cons of the result
  }

  unsafe_cell_remove(num);
  cell_remove_args(args);
  return res;
}

cell *concatenate(cell *args) {
#if CHECKS
  check_concatenate(args);
#endif
  cell *first_string = cadr(args);
  cell *second_string = caddr(args);
  char *first = first_string->str;
  char *second = second_string->str;
  char *new_str = malloc(sizeof(first) + sizeof(second) + 1);

  strcpy(new_str, first);
  strcat(new_str, second);

  unsafe_cell_remove(first_string);
  unsafe_cell_remove(second_string);
  cell_remove_args(args);

  return mk_str(new_str);
}

cell *append(cell *args) {
#if CHECKS
  check_append(args);
#endif
  cell *first_list = car(args);
  cell *second_list = cadr(args);
  cell *act = first_list;

  while (act && cdr(act))
    act = cdr(act);

  if (act)
    act->cdr = second_list;
  else
    first_list = second_list;

  cell_remove_args(args);
  return first_list;
}

cell *length(cell *args) {
#if CHECKS
  check_length(args);
#endif
  cell *act = car(args);
  cell *ret;

  if (act && is_str(act))
    ret = length_string(act);
  else
    ret = length_cons(act);

  unsafe_cell_remove(args);
  return ret;
}

cell *length_string(cell *string) {
  cell *ret = mk_num(strlen(string->str));
  unsafe_cell_remove(string);
  return ret;
}

cell *length_cons(cell *list) {
  cell *tmp;
  unsigned long len = 0;

  while (list) {
    len++;
    tmp = cdr(list);
    cell_remove_recursive(car(list)); // remove sublist
    unsafe_cell_remove(list);         // remove cons of the sublis
    list = tmp;
  }

  return mk_num(len);
}

cell *subseq(cell *args) {
#if CHECKS
  check_subseq(args);
#endif
  cell *str = car(args);
  cell *start = cadr(args);
  int start_index = start->value;

  if (start_index > strlen(str->str)) {
    cell_remove_recursive(args);
    return NULL;
  }

  if (cddr(args))
    return subseq_one_index(args, start_index);
  else
    return subseq_two_indices(args, start_index);
}

cell *subseq_one_index(cell *args, int start_index) {
  cell *str = car(args);
  cell *end = caddr(args);
  int e = end->value;

  char *substr = malloc(e - start_index + 1);
  strncpy(substr, str->str + start_index, e - start_index);
  *(substr + (e - start_index)) = '\0';
  cell *ret = mk_str(substr);
  free(substr);
  cell_remove_recursive(args);

  return ret;
}

cell *subseq_two_indices(cell *args, int start_index) {
  cell *str = car(args);
  int e = strlen(str->str);

  char *substr = malloc(e - start_index + 1);
  strncpy(substr, str->str + start_index, e - start_index);
  *(substr + (e - start_index)) = '\0';
  cell *ret = mk_str(substr);
  cell_remove_recursive(args);

  return ret;
}

/********************************************************************************
 *                                    Utility
 ********************************************************************************/

cell *set(cell *args) {
#if CHECKS
  check_set(args);
#endif
  cell *name = car(args);
  cell *act = memory->global_env;
  cell *prec = NULL;

  while (act) {
    if (eq(name, caar(act)))
      return set_change_existing_value(args, act);

    prec = act;
    act = cdr(act);
  }

  return set_add_new_value(args, prec);
}

cell *set_change_existing_value(cell *args, cell *pair) {
  cell *name = car(args);
  cell *new_val = cadr(args);
  cell_remove_recursive(car(pair)->cdr); // remove old val
  car(pair)->cdr = new_val;

  cell_push_recursive(new_val);
  unsafe_cell_remove(name);
  cell_remove_args(args);

  return cdar(pair);
}

cell *set_add_new_value(cell *args, cell *prec) {
  cell *name = car(args);
  cell *new_val = cadr(args);
  cell *pair = mk_cons(name, new_val);

  cell_push_recursive(pair);
  cell *new = cons(pair, NULL);
  if (prec)
    prec->cdr = new;
  else
    memory->global_env = new;

  unsafe_cell_remove(name);
  unsafe_cell_remove(pair);
  cell_remove_args(args);

  return new_val;
}

cell *write(cell *arg) {
#if CHECKS
  check_one_arg(arg);
#endif
  cell *to_be_printed = car(arg);
  printf(ANSI_COLOR_GRAY " > " ANSI_COLOR_RESET);
  print_sexpr(to_be_printed);
  puts("");
  cell_remove_args(arg);
  return to_be_printed;
}

cell *load(cell *arg, cell *env) {
#if CHECKS
  check_one_arg(arg);
#endif
  cell *name = eval(car(arg), env); // extract the name
#if CHECKS
  if (!name || !is_str(name))
    pi_lisp_error("first arg must me a string");
#endif
  FILE *file = fopen(((name) ? name->str : ""), "r");
  cell *last_result = NULL;

  if (!file)
    pi_lisp_error("can't find file");

  while (!feof(file)) {
    cell *sexpr = read_sexpr(file);
    if (sexpr != symbol_file_ended) {
      // eval only if you didn't read an empty fragment
      last_result = eval(sexpr, env);
      if (!feof(file))
        cell_remove_recursive(last_result);
    }
  }

  unsafe_cell_remove(name);
  cell_remove_args(arg);

  return last_result;
}

cell *bye(cell *arg) { return symbol_bye; }

/********************************************************************************
 *                                     Macros
 ********************************************************************************/

cell *quote(cell *args, cell *env) {
#if CHECKS
  check_one_arg(args);
#endif
  cell *evaulated = car(args);
  unsafe_cell_remove(args);
  return evaulated;
}

cell *cond(cell *arg, cell *env) { return evcon(arg, env); }

cell *setq(cell *args, cell *env) {
#if CHECKS
  check_setq(args);
#endif
  cell *sym = car(args);
  cell *val = eval(cadr(args), env);
  cell *ret = set(mk_cons(sym, mk_cons(val, NULL)));
  cell_remove_args(args);

  return ret;
}

cell *defun(cell *args, cell *env) {
  cell *fun_name = car(args);
  cell *lambda_struct = (cdr(args));
  cell *lambda_head = mk_cons(symbol_lambda, lambda_struct);
  cell *structure_for_set = mk_cons(fun_name, mk_cons(lambda_head, NULL));
  set(structure_for_set);
  cell_remove(args);

  return lambda_head;
}

cell *let(cell *args, cell *env) {
#if CHECKS
  check_two_args(args);
#endif
  cell *params = car(args);
  cell *body = cadr(args);
  cell *new_env = env;

  cell *val;
  cell *new_pair;
  cell *tmp;

  while (params) {
    val = eval(cadar(params), env);
    new_pair = mk_cons(caar(params), val);
    new_env = mk_cons(new_pair, new_env);

    tmp = cdr(params);
    cell_remove_let_param(params);
    params = tmp;
  }
  cell *res = eval(body, new_env);
  cell_remove_pairlis_deep(new_env, env);
  cell_remove_args(args);

  return res;
}

cell *dotimes(cell *arg, cell *env) {
#if CHECKS
  check_two_args(arg);
#endif
  int n = 0;
  cell *name_list = car(arg);
  cell *num = cadar(arg);
  cell *expr = cadr(arg);

  cell *new_env;
  cell *actual_n_value;
  cell *num_list_for_new_env;
  cell *evaulated;

  for (n = 0; n < num->value; n++) {
    if (n > 0)
      cell_push_recursive(expr);

    actual_n_value = mk_num(n);
    num_list_for_new_env = mk_cons(actual_n_value, NULL);
    new_env = pairlis(name_list, num_list_for_new_env, env);
    evaulated = eval(expr, new_env);

    cell_remove_recursive(evaulated);
    cell_remove_pairlis(new_env, env);
    cell_remove_recursive(num_list_for_new_env);
  }

  unsafe_cell_remove(cdr(arg));
  cell_remove(arg);
  cell_remove_recursive(car(arg));

  return NULL;
}

cell *map(cell *args, cell *env) {
  cell *func = car(args);
  cell *list = cadr(args);
  list = eval(list, env);
  cell *result = NULL;
  cell *last_added = NULL;
  cell *val;
  cell *element;
  cell *tmp;

  while (list) {
    cell_push_recursive(func);
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

  cell_remove_recursive(func);
  cell_remove_args(args);
  return result;
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

/********************************************************************************
 *                              Pilisp special functions
 ********************************************************************************/

cell *compile(cell *c, cell *env) {
#if CHECKS
  check_compile(c);
#endif
  cell *name = c->car;
  cell *to_compilate = eval(name, env);

  if (!should_be_compiled(to_compilate))
    return to_compilate;

  else {
    char *file_name = generate_pi_compile_tmp_file_name();
    write_compiler_expression_to_file(file_name, to_compilate);

    cell *compiled = load(mk_cons(mk_str(file_name), NULL), env);
    cell *new_env_pair = mk_cons(name, mk_cons(compiled, NULL));
    set(new_env_pair);

#if REMOVE_TMP_FILES
    remove(file_name);
#endif

    return compiled;
  }
}

bool should_be_compiled(cell *to_compilate) {
  // we compile only lambdas
  return is_cons(to_compilate) && eq(symbol_lambda, car(to_compilate));
}

cell *asm_call(cell *args, cell *env) {
  // this asm call has no params on the stack => we have to pass stack_pointer
  // as the base of the stack
  return asm_call_with_stack_base(args, env, stack_pointer);
}

cell *mem_dump(cell *arg) {
#if CHECKS
  check_zero_arg(arg);
#endif

  printf(ANSI_COLOR_YELLOW "============================== MEMORY "
                           "==============================\n" ANSI_COLOR_RESET);
  print_cell_space(memory);
  return symbol_true;
}

cell *env(cell *arg) {
#if CHECKS
  check_zero_arg(arg);
#endif

  printf(" > env: " ANSI_COLOR_BLUE);
  print_sexpr(memory->global_env);
  printf("\n" ANSI_COLOR_RESET);
  return symbol_true;
}

cell *collect_garbage_call(cell *arg) {
  deep_collect_garbage(memory);
  return symbol_true;
}

/********************************************************************************
 *                                  NOT CLEAN CODE
 ********************************************************************************/

bool total_eq(cell *c1, cell *c2) {
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

#if !INLINE_FUNCTIONS
cell *caar(cell *c) { return c->car->car; }
cell *cddr(cell *c) { return c->cdr->cdr; }
cell *cadr(cell *c) { return c->cdr->car; }
cell *cdar(cell *c) { return c->car->cdr; }
cell *cadar(cell *c) { return c->car->cdr->car; }
cell *caddr(cell *c) { return c->cdr->cdr->car; }
cell *cons(cell *car, cell *cdr) { return mk_cons(car, cdr); }

int atom(cell *c) {
  return (c == NULL) ||
         (c->type == TYPE_SYM || c->type == TYPE_NUM || c->type == TYPE_STR ||
          c->type == TYPE_BUILTINLAMBDA || c->type == TYPE_BUILTINMACRO ||
          c->type == TYPE_KEYWORD);
}

bool eq(cell *v1, cell *v2) {
  if (!v1 || !v2)
    return (v1 == v2);
  if (is_num(v1) && is_num(v2))
    return (v1->value == v2->value);
  if (is_str(v1) && is_str(v2))
    return (strcmp(v1->str, v2->str) == 0);
  return (v1 == v2);
}

cell *car(cell *c) {
  if (c == NULL)
    return NULL;
#if CHECKS
  if (atom(c))
    pi_lisp_error("car applied to an atom");
#endif
  return c->car;
}

cell *cdr(cell *c) {
  if (c == NULL)
    return NULL;
#if CHECKS
  if (atom(c))
    pi_lisp_error("cdr applied to an atom");
#endif
  return c->cdr;
}
#endif
