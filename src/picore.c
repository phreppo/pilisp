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
  while (x) {
    cell *left = car(x);
    cell *right = car(y);
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
  while (l) {
    // we extract the first element in the pair
    if (eq(x, car(car(l)))) {
      // right pair
      return l->car;
    }
    l = l->cdr;
  }
  return NULL;
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
      if (eq(car(fn), symbol_lambda)) {
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
        return res;
      }
      // LABEL
      if (eq(car(fn), symbol_label)) {
        if (eval_args)
          x = evlis(x, a);
        cell *new_env = cons(cons(cadr(fn), caddr(fn)), a);
        cell *res = apply(caddr(fn), x, new_env, false);
        return res;
      }

      if (eq(car(fn), symbol_macro)) {
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

      if (function_body == NULL)
        pi_error(LISP_ERROR, "unknown function ");
      if (!is_cons(function_body))
        pi_error(LISP_ERROR, "trying to apply a non-lambda");
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
          cell *pair = assoc(e, a);
          cell *symbol_value = cdr(pair);
          if (!pair) {
            // the symbol has no value in the env
            char *err = "unknown symbol ";
            char *sym_name = e->sym;
            char result[ERROR_MESSAGE_LEN];
            strcpy(result, err);
            strcat(result, sym_name);
            pi_error(LISP_ERROR, result);
          } else
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
      evaulated = car(e)->bm(cdr(e),a);
    }
    // ==================== SPECIAL FORMS ====================
     else  {
      if (eq(car(e), symbol_lambda) || eq(car(e), symbol_macro)) 
        // lambda and macro "autoquote"
        evaulated = e;
       else {
        // apply atom function to evaluated list of parameters
        cell *args = cdr(e);
        evaulated = apply(car(e), args, a, true);
      }
    }
  }

  //========================= COMPOSED FUNCTION EVAL =========================//
  //=========================   ((lambda (x) x) 1)   =========================//

  else {
    if ((eq(caar(e), symbol_macro))) {
      // MACRO
      cell *old_env = a;
      cell *body = car(e);
      cell *prm = cdr(e);
      a = pairlis(cadr(body), prm, a);
      cell *fn_body = caddr(body);
      evaulated = eval(fn_body, a);
    } else {
      // ==================== COMPOSED FUNCTION ====================
      evaulated = apply(car(e), cdr(e), a, true);
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
    // eval the bod of the cond
    res = eval(cadar(c), a);
  } else {
    res = evcon(cdr(c), a);
  }

  return res;
}
