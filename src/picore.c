#include "picore.h"

cell *pairlis(cell *x, cell *y, cell *a) {
#if DEBUG_EVAL_MODE
  printf("Pairlis:\t" ANSI_COLOR_GREEN);
  print_sexpr(x);
  printf(ANSI_COLOR_RESET " wiht: " ANSI_COLOR_GREEN);
  print_sexpr(y);
#if DEBUG_EVAL_PRINT_ENV_MODE
  printf(ANSI_COLOR_RESET " in the env " ANSI_COLOR_DARK_GRAY);
  print_sexpr(a);
#endif
  printf(ANSI_COLOR_RESET "\n");
#endif
  cell *result = a;
  cell *left;
  cell *right;
  while (x) {
    left = car(x);
    right = car(y);

    // NEW
    add_symbol_value(left, right);

    cell *new_pair = mk_cons(left, right);
    result = mk_cons(new_pair, result);
    x = cdr(x);
    y = cdr(y);
  }
#if DEBUG_EVAL_MODE
  printf("Parilis res:\t" ANSI_COLOR_BLUE);
  print_sexpr(result);
  printf(ANSI_COLOR_RESET "\n");
#endif
  return result;
}

cell *assoc(const cell *x, cell *l) {
  if (!x->value_list) {
      // the symbol has no value in the env
      char *err = "unknown symbol ";
      char *sym_name = x->sym;
      char result[ERROR_MESSAGE_LEN];
      strcpy(result, err);
      strcat(result, sym_name);
      pi_error(LISP_ERROR, result);
  }
  cell_push(x->value_list->assoc, RECURSIVE); // protect the value. if it s a
                                              // list protect all the members
  cell_remove(x, SINGLE);
  return x->value_list->assoc;
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
      //========================= ATOM FUNCTION =========================//
      //=========================    (fun x)    =========================//

      if (fn->type == TYPE_BUILTINLAMBDA) { // BASIC OPERATIONS
        if (eval_args)
          x = evlis(x, a);

        return fn->bl(x);

      } else {
        // CUSTOM FUNCTION
        // does lambda exists?
        cell *function_body = eval(fn, a);
        if (function_body == NULL)
          pi_error(LISP_ERROR, "unknown function ");
        if (!is_cons(function_body))
          pi_error(LISP_ERROR, "trying to apply a non-lambda");
        if ((car(function_body) != symbol_macro) && eval_args)
          // eval args only if it s not a macro
          x = evlis(x, a);
        // the env knows the lambda
        cell *ret = apply(function_body, x, a, false);
        return ret;
      }

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
        // NEW
        pop_pairlis(cadr(fn));            // remove the last assoc
        cell_remove_cars(x);              // deep remove cars
        cell_remove_args(x);              // remove args cons
        cell_remove_pairlis(a, old_env);  // remove associations
        cell_remove(car(fn), SINGLE);     // function name
        cell_remove(cadr(fn), RECURSIVE); // params
        cell_remove(cddr(fn), SINGLE);    // cons pointing to body
        cell_remove(cdr(fn), SINGLE);     // cons poining to param
        cell_remove(fn, SINGLE);          // cons pointing to lambda sym
        return res;
      }
      // LABEL
      if (car(fn) == symbol_label) {
        if (eval_args)
          x = evlis(x, a);
        cell *new_env = cons(cons(cadr(fn), caddr(fn)), a);
        add_symbol_value(cadr(fn), caddr(fn));
        cell *res = apply(caddr(fn), x, new_env, false);
        pop_symbol_value(cadr(fn));
        cell_remove(cddr(fn), SINGLE); // cons of the body
        cell_remove(cadr(fn), SINGLE); // symbol to bind
        cell_remove(cdr(fn), SINGLE);  // cons of the top level
        cell_remove(car(fn), SINGLE);  // symbol label
        cell_remove(fn, SINGLE);       // cons of everything
        cell_remove(car(new_env),
                    SINGLE);          // new cons of the pair of the new env
        cell_remove(new_env, SINGLE); // head of new env
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
        pop_pairlis(cadr(fn));            // remove the last assoc
        cell_remove_cars(x);              // deep remove cars
        cell_remove_pairlis(a, old_env);  // remove associations
        cell_remove(car(fn), SINGLE);     // function name
        cell_remove(cadr(fn), RECURSIVE); // params
        cell_remove(cddr(fn), SINGLE);    // cons pointing to body
        cell_remove(cdr(fn), SINGLE);     // cons poining to param
        cell_remove(fn, SINGLE);          // cons pointing to lambda sym
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
      if (is_num(e) || is_str(e))
        // VALUE
        evaulated = e;
      else {
        // it's a symbol: we have to search for that
        if (e == symbol_true)
          evaulated = symbol_true;
        else {
          // cell *pair = assoc(e, a);
          // cell *symbol_value = cdr(pair);
          cell *symbol_value = assoc(e, a);
          // if (!pair) {
          //   // the symbol has no value in the env
          //   char *err = "unknown symbol ";
          //   char *sym_name = e->sym;
          //   char result[ERROR_MESSAGE_LEN];
          //   strcpy(result, err);
          //   strcat(result, sym_name);
          //   pi_error(LISP_ERROR, result);
          // } else
          //   // the symbol has a value in the env
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
      cell_remove(e, SINGLE);
    }
    // ==================== SPECIAL FORMS ====================
    else {
      // ==
      if ((car(e) == symbol_lambda) || (car(e) == symbol_macro))
        // lambda and macro "autoquote"
        evaulated = e;
      else {
        // apply atom function to evaluated list of parameters
        cell *args = cdr(e);
        evaulated = apply(car(e), args, a, true);
        cell_remove(e, SINGLE);   // remove function
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

      pop_pairlis(cadr(body)); // remove the last assoc
      cell_remove_pairlis(a, old_env);
      cell_remove(cdr(e), RECURSIVE);    // params tree
      cell_remove(cdr(cdar(e)), SINGLE); // cons of the body
      cell_remove(cadar(e), RECURSIVE);  // formal params
      cell_remove(cdar(e), SINGLE);      // cons of params
      cell_remove(caar(e), SINGLE);      // symbol macro
      cell_remove(car(e), SINGLE);       // cons of macro
      cell_remove(e, SINGLE);            // head of everything
    } else {
      // ==================== COMPOSED FUNCTION ====================
      evaulated = apply(car(e), cdr(e), a, true);
      cell_remove(e, SINGLE); // remove function
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

  if (res != NULL) {
    // result of the last eval
    cell_remove(res, RECURSIVE);
    // eval the bod of the cond
    res = eval(cadar(c), a);
    // cut off the rest of the sexpressions
    cell_remove(cdr(c), RECURSIVE);

  } else {
    // result of the last eval
    cell_remove(res, RECURSIVE);
    // remove the unevaluated body
    cell_remove(cadar(c), RECURSIVE);
    res = evcon(cdr(c), a);
  }
  // cons of the body
  cell_remove(cdar(c), SINGLE);
  // cons of the pair (cond [body])
  cell_remove(car(c), SINGLE);
  // head of the list
  cell_remove(c, SINGLE);

  return res;
}
