/** @addtogroup pibuiltin */
/*@{*/
#ifndef PIBUILTIN_H
#define PIBUILTIN_H
#include "picell.h"
#include "pilisp.h"
#include <stdbool.h>
#include <time.h>

// ==================== BASIC ====================
cell *cons(cell *car, cell *cdr);
inline int atom(const cell *c) {
  return (c == NULL) // NIL case
         ||
         (c->type == TYPE_SYM || c->type == TYPE_NUM || c->type == TYPE_STR ||
          c->type == TYPE_BUILTINLAMBDA || c->type == TYPE_BUILTINMACRO);
}
inline bool eq(const cell *v1, const cell *v2) {
  if (!v1 || !v2)
    return (v1 == v2);
  if (is_num(v1) && is_num(v2))
    return (v1->value == v2->value);
  if (is_str(v1) && is_str(v2))
    return (strcmp(v1->str, v2->str) == 0);
  return (v1 == v2);
}

bool total_eq(const cell *c1,
              const cell *c2); // works also on lists: eq does not

inline cell *car(const cell *c) {
  if (c == NULL)
    // (car NIL)
    return NULL;
#if CHECKS
  if (atom(c))
    pi_lisp_error("car applied to an atom");
#endif
  return c->car;
}

inline cell *cdr(const cell *c) {
  if (c == NULL)
    // (cdr NIL)
    return NULL;
#if CHECKS
  if (atom(c))
    pi_lisp_error("cdr applied to an atom");
#endif
  return c->cdr;
}

// not usable in the interpreter: no checks! => use only in the eval
inline cell *caar(const cell *c) { return c->car->car; }
inline cell *cddr(const cell *c) { return c->cdr->cdr; }
inline cell *cadr(const cell *c) { return c->cdr->car; }
inline cell *cdar(const cell *c) { return c->car->cdr; }
inline cell *cadar(const cell *c) { return c->car->cdr->car; }
inline cell *caddr(const cell *c) { return c->cdr->cdr->car; }

// ==================== BASIC APPLY ====================
// differences from the first basic block: these functions can be called from
// the apply, because they do cell_remove and cell_push and check for args error
cell *builtin_car(const cell *args);
cell *builtin_cdr(const cell *args);
cell *builtin_cons(const cell *args);
cell *builtin_atom(const cell *args);
cell *builtin_eq(const cell *args);

// ==================== LOGIC ====================
cell * or (const cell *operands);
cell * and (const cell *operands);
cell * not(const cell *operands);

// ==================== COMPARISON ====================
cell *greater(const cell *operands);
cell *greater_eq(const cell *operands);
cell *less(const cell *operands);
cell *less_eq(const cell *operands);
cell *integerp(const cell *arg);
cell *symbolp(const cell *arg);

// ==================== ARITHMETIC ====================
cell *addition(const cell *numbers);
cell *subtraction(const cell *numbers);
cell *multiplication(const cell *numbers);
cell *division(const cell *numbers);

// ==================== UTILITY ====================
cell *set(cell *args);
cell *load(cell *arg, cell *env);
cell *write(cell *arg);
cell *bye(cell *arg);
cell *mem_dump(cell *arg);
cell *env(cell *arg);
cell *collect_garbage_call(cell *arg);

// ==================== LISTS ====================
cell *length(const cell *list);
cell *member(const cell *list);
cell *nth(const cell *list);
cell *list(const cell *list);
cell *subseq(const cell *list); // substr
cell *reverse(const cell *list);

// ==================== MACROS ====================
cell *setq(const cell *args, cell *env);
cell *defun(const cell *args, cell *env);
cell *let(const cell *args, cell *env);
cell *map(const cell *args, cell *env);
cell *quote(const cell *args, cell *env);
cell *timer(cell *arg, cell *env);
cell *cond(const cell *arg, cell *env);
cell *dotimes(const cell *arg, cell *env);

#endif // !PIBUILTIN_H
       /*@}*/