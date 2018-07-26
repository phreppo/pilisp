#include "picore.h"
#include "piremove.h"

cell *eval(cell *expression, cell *env) {

  if (atom(expression))
    return eval_atom(expression, env);

  if (atom(car(expression)))
    return eval_atom_function(expression, env);

  return eval_composed_function(expression, env);
}

cell *eval_atom(cell *expression, cell *env) {
  if (!expression)
    return NULL;

  if (is_num(expression) || is_str(expression) || is_keyword(expression))
    return expression;

  if (expression == symbol_true)
    return symbol_true;

  // it's a symbol: we have to search for that
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
  }
#endif
  // the symbol has a value in the env
  return symbol_value;
}

cell *eval_atom_function(cell *expression, cell *env) {
  cell *evaluated = NULL;
  cell *function_symbol = car(expression);
  cell *args = cdr(expression);

  if (is_builtin_macro(function_symbol))
    evaluated = function_symbol->bm(args, env);
  else {
    if (function_symbol == symbol_lambda || function_symbol == symbol_macro ||
        function_symbol == symbol_lasm)
      // "autoquote"
      evaluated = expression;
    else {
      // apply atom function to evaluated list of parameters
      evaluated = apply(function_symbol, args, env, true);
      cell_remove_args(args); // remove list of args
    }
  }
  unsafe_cell_remove(expression); // cons of the expression
  return evaluated;
}

cell *eval_composed_function(cell *expression, cell *env) {
  cell *evaluated = NULL;

  if (caar(expression) == symbol_macro)
    evaluated = eval_macro(expression, env);
  else {
    evaluated = apply(car(expression), cdr(expression), env, true);
    cell_remove_args(cdr(expression));
    unsafe_cell_remove(expression); // remove function
  }

  return evaluated;
}

cell *eval_macro(cell *expression, cell *env) {
  cell *evaluated = NULL;
  cell *old_env = env;
  cell *body = car(expression);
  cell *params = cdr(expression);
  cell *fn_body = caddr(body);

  env = pairlis(cadr(body), params, env);
  evaluated = eval(fn_body, env);
  cell_remove_eval_macro(env, old_env, expression);
  return evaluated;
}

cell *apply(cell *fn, cell *args, cell *env, bool eval_args) {
  if (atom(fn))
    return apply_atom_function(fn, args, env, eval_args);
  else
    return apply_composed_function(fn, args, env, eval_args);
}

cell *evlis(cell *args, cell *env) {
  if (!args)
    return NULL;
  cell *valued_car = eval(car(args), env);
  cell *valued_cdr = evlis(cdr(args), env);
  return mk_cons(valued_car, valued_cdr);
}

cell *evcon(cell *args, cell *env) {
  cell *res = eval(caar(args), env);
  cell *ret;

  if (res != NULL) {
    ret = eval(cadar(args), env);
    cell_remove_recursive(res);       // result of the last eval
    cell_remove_recursive(cdr(args)); // cut off the rest of the sexpressions
  } else {
    ret = evcon(cdr(args), env);
    cell_remove_recursive(res);         // result of the last eval
    cell_remove_recursive(cadar(args)); // remove the unevaluated body
  }

  unsafe_cell_remove(cdar(args)); // cons of the body
  unsafe_cell_remove(car(args));  // cons of the pair (cond [body])
  unsafe_cell_remove(args);       // head of the list

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
      cell_push_recursive(cdar(env)); // protect the value of the symbol
      unsafe_cell_remove(symbol);     // symbol was used
      result = env->car;
    }
    env = env->cdr;
  }

  return result;
}

cell *apply_atom_function(cell *fn, cell *args, cell *env, bool eval_args) {
  if (is_builtin_lambda(fn)) {
    // BASIC OPERATIONS
    if (eval_args)
      args = evlis(args, env);
    if (fn)
      return fn->bl(args);
    else 
      exit(1);
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
  cell_remove_lambda(env, old_env, args, fn);
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
  cell *res = asm_call_with_stack_base(cddr(fn), env, stack_base);
  stack_pointer = stack_pointer - cadr(fn)->value;
#if CHECKS
  if (stack_pointer != stack_base)
    pi_error_stack();
#endif
  unsafe_cell_remove(cadr(fn));
  unsafe_cell_remove(cdr(fn));
  cell_remove_args(args);
  unsafe_cell_remove(fn); // cons of the lasm
  return res;
}

cell *apply_label(cell *fn, cell *args, cell *env, bool eval_args) {
  if (eval_args)
    args = evlis(args, env);
  cell *new_env = cons(cons(cadr(fn), caddr(fn)), env);
  cell *res = apply(caddr(fn), args, new_env, false);
  cell_remove_label(new_env, fn);
  return res;
}

cell *apply_macro(cell *fn, cell *args, cell *env, bool eval_args) {
  cell *old_env = env;
  env = pairlis(cadr(fn), args, env);
  cell *fn_body = caddr(fn);
  cell *res = eval(fn_body, env);
  res = eval(res, env);
  cell_remove_apply_macro(env, old_env, args, fn);
  return res;
}

cell *eval_lambda_and_apply(cell *fn, cell *args, cell *env, bool eval_args) {
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