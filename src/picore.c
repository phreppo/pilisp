#include "picore.h"

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

cell *assoc(const cell *symbol, cell *env) {
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

cell *apply(cell *fn, cell *x, cell *a, bool eval_args) {

#if DEBUG_EVAL_MODE
  printf("Applying:\t" ANSI_COLOR_GREEN);
  print_sexpr(fn);
  printf(ANSI_COLOR_RESET " to: " ANSI_COLOR_BLUE);
  print_sexpr(x);
#if DEBUG_EVAL_PRINT_ENV_MODE
  printf(ANSI_COLOR_RESET " in the env: " ANSI_COLOR_DARK_GRAY);
  print_sexpr(a);
#endif
  printf(ANSI_COLOR_RESET "\n");
#endif
  if (fn) {
    if (atom(fn)) {
      return apply_atom_function(fn, x, a, eval_args);
    } else {
      //========================= COMPOSED FUNCTION =========================//
      //================= ( (lambda (x y z) (....)) param) ==================//
      if (car(fn) == symbol_lambda) {
        // direct lambda
#if DEBUG_EVAL_MODE
        printf("LAMBDA:\t\t" ANSI_COLOR_RED);
        print_sexpr(fn);
        printf(ANSI_COLOR_RESET "\n");
#endif
        if (eval_args)
          x = evlis(x, a);
        cell *old_env = a;
        a = pairlis(cadr(fn), x, a);
        cell *fn_body = caddr(fn);
        cell *res = eval(fn_body, a);
        // FREE THINGS
        cell_remove_recursive(a->car->cdr);
        cell_remove_cars(x);                  // deep remove cars
        cell_remove_args(x);                  // remove args cons
        cell_remove_pairlis_deep(a, old_env); // remove associations
        unsafe_cell_remove(car(fn));          // function name
        cell_remove_recursive(cadr(fn));      // params
        unsafe_cell_remove(cddr(fn));         // cons pointing to body
        unsafe_cell_remove(cdr(fn));          // cons poining to param
        unsafe_cell_remove(fn);               // cons pointing to lambda sym
        return res;
      }
      // LASM
      if (car(fn) == symbol_lasm) {
        // CALL LASM
        if (eval_args)
          x = evlis(x, a);
        cell *act_arg = x;
        // we save the base of our stack
        size_t stack_base = stack_pointer;
        while (act_arg) {
          // put everything on the stack
          stack_push(act_arg->car);
          act_arg = act_arg->cdr;
        }
        return asm_call_with_stack_base(cddr(fn), stack_base);
      }
      // LABEL
      if (eq(car(fn), symbol_label)) {
        if (eval_args)
          x = evlis(x, a);
        cell *new_env = cons(cons(cadr(fn), caddr(fn)), a);
        cell *res = apply(caddr(fn), x, new_env, false);
        unsafe_cell_remove(cddr(fn));     // cons of the body
        unsafe_cell_remove(cadr(fn));     // symbol to bind
        unsafe_cell_remove(cdr(fn));      // cons of the top level
        unsafe_cell_remove(car(fn));      // symbol label
        unsafe_cell_remove(fn);           // cons of everything
        unsafe_cell_remove(car(new_env)); // new cons of the pair of the new env
        cell_remove(new_env);             // head of new env
        return res;
      }

      if (car(fn) == symbol_macro) {
        // ==================== (MACRO ...) ====================
#if DEBUG_EVAL_MODE
        printf("MACRO:\t\t" ANSI_COLOR_RED);
        print_sexpr(fn);
        printf(ANSI_COLOR_RESET "\n");
#endif
        cell *old_env = a;
        a = pairlis(cadr(fn), x, a);
        cell *fn_body = caddr(fn);
        cell *res = eval(fn_body, a);
        res = eval(res, a); // raises a buggerino
        // FREE THINGS
        cell_remove_cars(x);                  // deep remove cars
        cell_remove_pairlis_deep(a, old_env); // remove associations
        unsafe_cell_remove(car(fn));          // function name
        cell_remove_recursive(cadr(fn));      // params
        unsafe_cell_remove(cddr(fn));         // cons pointing to body
        unsafe_cell_remove(cdr(fn));          // cons poining to param
        unsafe_cell_remove(fn);               // cons pointing to lambda sym
        return res;
      }

#if DEBUG_EVAL_MODE
      printf("Resolving fun: \t" ANSI_COLOR_RED);
      print_sexpr(fn);
      printf(ANSI_COLOR_RESET "\n");
#endif
      // function is not an atomic function: something like (lambda (x) (lambda
      // (y) y))
      if (eval_args)
        x = evlis(x, a);

      cell *function_body = eval(fn, a);
#if CHECKS
      if (function_body == NULL)
        pi_error(LISP_ERROR, "unknown function ");
      if (!is_cons(function_body))
        pi_error(LISP_ERROR, "trying to apply a non-lambda");
#endif
      // the env knows the lambda
      return apply(function_body, x, a, false);
    }
  }
  return NULL; // error?
}

cell *eval(cell *e, cell *a) {
#if DEBUG_EVAL_MODE
  printf("Evaluating: \t" ANSI_COLOR_GREEN);
  print_sexpr(e);
#if DEBUG_EVAL_PRINT_ENV_MODE
  printf(ANSI_COLOR_RESET " in the env: " ANSI_COLOR_DARK_GRAY);
  print_sexpr(a);
#endif
  printf(ANSI_COLOR_RESET "\n");
#endif
  cell *evaulated = NULL;
  //========================= ATOM EVAL =========================//
  // ** every used cells released **
  if (atom(e)) {
    if (!e)
      // NIL
      evaulated = NULL;
    else {
      if (is_num(e) || is_str(e) || is_keyword(e))
        // VALUE
        evaulated = e;
      else {
        // it's a symbol: we have to search for that
        if (e == symbol_true)
          evaulated = symbol_true;
        else {
          cell *pair = assoc(e, a);
          cell *symbol_value = cdr(assoc(e, a));
#if CHECKS
          if (!pair) {
            // the symbol has no value in the env
            char *err = "unknown symbol ";
            char *sym_name = e->sym;
            char result[ERROR_MESSAGE_LEN];
            strcpy(result, err);
            strcat(result, sym_name);
            pi_error(LISP_ERROR, result);
          } else
#endif
            // the symbol has a value in the env
            evaulated = symbol_value;
        }
      }
    }
  }
  //========================= ATOM FUNCTION EVAL =========================//
  else if (atom(car(e))) {
    if (is_builtin_macro(car(e))) {
      // ==================== BUILTIN MACRO ====================
      evaulated = car(e)->bm(cdr(e), a);
      unsafe_cell_remove(e); // cons of the expression
    }
    // ==================== SPECIAL FORMS ====================
    else {
      if (car(e) == symbol_lambda || car(e) == symbol_macro ||
          car(e) == symbol_lasm)
        // lambda and macro "autoquote"
        evaulated = e;
      else {
        // apply atom function to evaluated list of parameters
        cell *args = cdr(e);
        evaulated = apply(car(e), args, a, true);
        unsafe_cell_remove(e);    // remove cons of the expression
        cell_remove_args(cdr(e)); // remove list of args
      }
    }
  }

  //========================= COMPOSED FUNCTION EVAL =========================//
  //=========================   ((lambda (x) x) 1)   =========================//

  else {
    if (caar(e) == symbol_macro) {
      // MACRO
      cell *old_env = a;
      cell *body = car(e);
      cell *prm = cdr(e);
      a = pairlis(cadr(body), prm, a);
      cell *fn_body = caddr(body);
      evaulated = eval(fn_body, a);

      cell_remove_pairlis(a, old_env);
      cell_remove_recursive(cdr(e));    // params tree
      unsafe_cell_remove(cdr(cdar(e))); // cons of the body
      cell_remove_recursive(cadar(e));  // formal params
      unsafe_cell_remove(cdar(e));      // cons of params
      cell_remove(caar(e));             // symbol macro
      unsafe_cell_remove(car(e));       // cons of macro
      unsafe_cell_remove(e);            // head of everything
    } else {
      // ==================== COMPOSED FUNCTION ====================
      evaulated = apply(car(e), cdr(e), a, true);
      unsafe_cell_remove(e); // remove function
      cell_remove_args(cdr(e));
    }
  }
#if DEBUG_EVAL_MODE
  printf("Evaluated: \t" ANSI_COLOR_GREEN);
  print_sexpr(e);
  printf(ANSI_COLOR_RESET " to: " ANSI_COLOR_LIGHT_BLUE);
  print_sexpr(evaulated);
  printf(ANSI_COLOR_RESET "\n");
#endif
  return evaulated;
}

cell *evlis(cell *m, cell *a) {
#if DEBUG_EVAL_MODE
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

  cell *res = eval(caar(c), a);
  cell *ret;
  if (res != NULL) {
    ret = eval(cadar(c), a);
    // result of the last eval
    cell_remove_recursive(res);
    // eval the bod of the cond
    // cut off the rest of the sexpressions
    cell_remove_recursive(cdr(c));

  } else {
    ret = evcon(cdr(c), a);

    // result of the last eval
    cell_remove_recursive(res);
    // remove the unevaluated body
    cell_remove_recursive(cadar(c));
  }
  // cons of the body
  unsafe_cell_remove(cdar(c));
  // cons of the pair (cond [body])
  unsafe_cell_remove(car(c));
  // head of the list
  unsafe_cell_remove(c);

  return ret;
}

// ==================== Support functions ====================

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