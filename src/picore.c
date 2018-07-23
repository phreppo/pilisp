#include "picore.h"

cell *eval(cell *expression, cell *env) {

  if (atom(expression))
    return eval_atom(expression, env);

  if (atom(car(expression)))
    return eval_atom_function(expression, env);

  return eval_composed_function(expression, env);
}

cell *eval_atom(cell *expression, cell *env) {
  if (!expression)
    // NIL
    return NULL;
  else {
    if (is_num(expression) || is_str(expression) || is_keyword(expression))
      // VALUE
      return expression;
    else {
      // it's a symbol: we have to search for that
      if (expression == symbol_true)
        return symbol_true;
      else {
        cell *pair = assoc(expression, env);
        cell *symbol_value = cdr(assoc(expression, env));
#if CHECKS
        if (!pair) {
          // the symbol has no value in the env
          char *err = "unknown symbol ";
          char *sym_name = expression->sym;
          char result[ERROR_MESSAGE_LEN];
          strcpy(result, err);
          strcat(result, sym_name);
          pi_error(LISP_ERROR, result);
        } else
#endif
          // the symbol has a value in the env
          return symbol_value;
      }
    }
  }
}

cell *eval_atom_function(cell *expression, cell *env) {
  cell *evaluated = NULL;
  if (is_builtin_macro(car(expression))) {
    // ==================== BUILTIN MACRO ====================
    evaluated = car(expression)->bm(cdr(expression), env);
    unsafe_cell_remove(expression); // cons of the expression
  }
  // ==================== SPECIAL FORMS ====================
  else {
    if (car(expression) == symbol_lambda || car(expression) == symbol_macro ||
        car(expression) == symbol_lasm)
      // lambda and macro "autoquote"
      evaluated = expression;
    else {
      // apply atom function to evaluated list of parameters
      cell *args = cdr(expression);
      evaluated = apply(car(expression), args, env, true);
      unsafe_cell_remove(expression);    // remove cons of the expression
      cell_remove_args(cdr(expression)); // remove list of args
    }
  }
  return evaluated;
}

cell *eval_composed_function(cell *expression, cell *env) {
  cell *evaluated = NULL;

  if (caar(expression) == symbol_macro) {
    // MACRO
    cell *old_env = env;
    cell *body = car(expression);
    cell *prm = cdr(expression);
    env = pairlis(cadr(body), prm, env);
    cell *fn_body = caddr(body);
    evaluated = eval(fn_body, env);

    cell_remove_pairlis(env, old_env);
    cell_remove_recursive(cdr(expression));    // params tree
    unsafe_cell_remove(cdr(cdar(expression))); // cons of the body
    cell_remove_recursive(cadar(expression));  // formal params
    unsafe_cell_remove(cdar(expression));      // cons of params
    cell_remove(caar(expression));             // symbol macro
    unsafe_cell_remove(car(expression));       // cons of macro
    unsafe_cell_remove(expression);            // head of everything
  } else {
    // ==================== COMPOSED FUNCTION ====================
    evaluated = apply(car(expression), cdr(expression), env, true);
    unsafe_cell_remove(expression); // remove function
    cell_remove_args(cdr(expression));
  }

  return evaluated;
}

cell *apply(cell *fn, cell *args, cell *env, bool eval_args) {
  if (atom(fn)) 
    return apply_atom_function(fn, args, env, eval_args);
  else 
    return apply_composed_function(fn, args, env, eval_args);
}

cell *evlis(cell *args_list, cell *env) {
  if (!args_list)
    return NULL;
  cell *valued_car = eval(car(args_list), env);
  cell *valued_cdr = evlis(cdr(args_list), env);
  return mk_cons(valued_car, valued_cdr);
}

cell *evcon(cell *args, cell *env) {

  cell *res = eval(caar(args), env);
  cell *ret;

  if (res != NULL) {
    ret = eval(cadar(args), env);
    // result of the last eval
    cell_remove_recursive(res);
    // cut off the rest of the sexpressions
    cell_remove_recursive(cdr(args));

  } else {
    ret = evcon(cdr(args), env);

    // result of the last eval
    cell_remove_recursive(res);
    // remove the unevaluated body
    cell_remove_recursive(cadar(args));
  }

  // cons of the body
  unsafe_cell_remove(cdar(args));
  // cons of the pair (cond [body])
  unsafe_cell_remove(car(args));
  // head of the list
  unsafe_cell_remove(args);

  return ret;
}

cell *pairlis(cell *symbols_list, cell *values_list, cell *a) {
  cell *result = a;
  cell *symbol;
  cell *value;
  cell *new_pair;

  while (symbols_list) {
    symbol = car(symbols_list);
    value = car(values_list);
    new_pair = mk_cons(symbol, value);
    result = mk_cons(new_pair, result);

    symbols_list = cdr(symbols_list);
    values_list = cdr(values_list);
  }
  return result;
}

cell *assoc(cell *symbol, cell *env) {
  cell *actual_symbol;
  cell *result = NULL;

  while (env && !result) {
    actual_symbol = caar(env);
    if (eq(symbol, actual_symbol)) {
      // protect the value of the symbol
      cell_push_recursive(cdar(env));
      // symbol was used
      unsafe_cell_remove(symbol);
      result = env->car;
    }
    env = env->cdr;
  }

  return result;
}


cell *apply_atom_function(cell *fn, cell *args, cell *env, bool eval_args) {
  if (fn->type == TYPE_BUILTINLAMBDA) {
    // BASIC OPERATIONS
    if (eval_args)
      args = evlis(args, env);
    return fn->bl(args);
  } else {
    // CUSTOM FUNCTION
    cell *function_body = eval(fn, env);
#if CHECKS
    if (function_body == NULL)
      pi_error(LISP_ERROR, "unknown function ");
    if (!is_cons(function_body))
      pi_error(LISP_ERROR, "trying to apply a non-lambda");
#endif
    if ((car(function_body) != symbol_macro) && eval_args)
      // eval args only if it s not a macro
      args = evlis(args, env);
    // the env knows the lambda
    return apply(function_body, args, env, false);
  }
}

cell *apply_composed_function(cell *fn, cell *args, cell *env, bool eval_args) {
  if (car(fn) == symbol_lambda)
    return apply_lambda(fn, args, env, eval_args);

  if (car(fn) == symbol_lasm)
    return apply_lasm(fn, args, env, eval_args);

  if (eq(car(fn), symbol_label))
    return apply_label(fn, args, env, eval_args);

  if (car(fn) == symbol_macro)
    return apply_macro(fn, args, env, eval_args);

  return eval_lambda_and_apply(fn, args, env, eval_args);
}

cell *apply_lambda(cell *fn, cell *args, cell *env, bool eval_args) {
  if (eval_args)
    args = evlis(args, env);
  cell *old_env = env;
  env = pairlis(cadr(fn), args, env);
  cell *fn_body = caddr(fn);
  cell *res = eval(fn_body, env);
  // FREE THINGS
  cell_remove_recursive(env->car->cdr);
  cell_remove_cars(args);                 // deep remove cars
  cell_remove_args(args);                 // remove args cons
  cell_remove_pairlis_deep(env, old_env); // remove associations
  unsafe_cell_remove(car(fn));            // function name
  cell_remove_recursive(cadr(fn));        // params
  unsafe_cell_remove(cddr(fn));           // cons pointing to body
  unsafe_cell_remove(cdr(fn));            // cons poining to param
  unsafe_cell_remove(fn);                 // cons pointing to lambda sym
  return res;
}

cell *apply_lasm(cell *fn, cell *args, cell *env, bool eval_args) {
  if (eval_args)
    args = evlis(args, env);
  cell *act_arg = args;
  // we save the base of our stack
  size_t stack_base = stack_pointer;
  while (act_arg) {
    // put everything on the stack
    stack_push(act_arg->car);
    act_arg = act_arg->cdr;
  }
  return asm_call_with_stack_base(cddr(fn), stack_base);
}

cell *apply_label(cell *fn, cell *args, cell *env, bool eval_args) {
  if (eval_args)
    args = evlis(args, env);
  cell *new_env = cons(cons(cadr(fn), caddr(fn)), env);
  cell *res = apply(caddr(fn), args, new_env, false);
  unsafe_cell_remove(cddr(fn));     // cons of the body
  unsafe_cell_remove(cadr(fn));     // symbol to bind
  unsafe_cell_remove(cdr(fn));      // cons of the top level
  unsafe_cell_remove(car(fn));      // symbol label
  unsafe_cell_remove(fn);           // cons of everything
  unsafe_cell_remove(car(new_env)); // new cons of the pair of the new env
  cell_remove(new_env);             // head of new env
  return res;
}

cell *apply_macro(cell *fn, cell *args, cell *env, bool eval_args) {
#if DEBUG_EVAL_MODE
  printf("MACRO:\t\t" ANSI_COLOR_RED);
  print_sexpr(fn);
  printf(ANSI_COLOR_RESET "\n");
#endif
  cell *old_env = env;
  env = pairlis(cadr(fn), args, env);
  cell *fn_body = caddr(fn);
  cell *res = eval(fn_body, env);
  res = eval(res, env);
  // FREE THINGS
  cell_remove_cars(args);                 // deep remove cars
  cell_remove_pairlis_deep(env, old_env); // remove associations
  unsafe_cell_remove(car(fn));            // function name
  cell_remove_recursive(cadr(fn));        // params
  unsafe_cell_remove(cddr(fn));           // cons pointing to body
  unsafe_cell_remove(cdr(fn));            // cons poining to param
  unsafe_cell_remove(fn);                 // cons pointing to lambda sym
  return res;
}

cell *eval_lambda_and_apply(cell *fn, cell *args, cell *env, bool eval_args) {
#if DEBUG_EVAL_MODE
  printf("Resolving fun: \t" ANSI_COLOR_RED);
  print_sexpr(fn);
  printf(ANSI_COLOR_RESET "\n");
#endif

  if (eval_args)
    args = evlis(args, env);

  cell *function_body = eval(fn, env);
#if CHECKS
  if (function_body == NULL)
    pi_error(LISP_ERROR, "unknown function ");
  if (!is_cons(function_body))
    pi_error(LISP_ERROR, "trying to apply env non-lambda");
#endif
  // the env knows the lambda
  return apply(function_body, args, env, false);
}