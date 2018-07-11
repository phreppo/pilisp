/** @addtogroup pibuiltin */
/*@{*/
#ifndef PIBUILTIN_H
#define PIBUILTIN_H
#include "picell.h"
#include "pilisp.h"
#include <stdbool.h>
#include <time.h>

// ==================== BASIC ====================
cell *caar(const cell *c);
cell *cadr(const cell *c);
cell *cdar(const cell *c);
cell *cddr(const cell *c);
cell *cadar(const cell *c);
cell *caddr(const cell *c);
cell *cons(cell *car, cell *cdr);
int atom(const cell *c);
bool eq(const cell *v1, const cell *v2);
bool total_eq(const cell *c1,
              const cell *c2); // works also on lists: eq does not

inline cell *car(const cell *c) {
  if (c == NULL)
    // (car NIL)
    return NULL;
#if CHECKS
  if (atom(c))
    pi_error(LISP_ERROR, "car applied to an atom");
#endif
  return c->car;
}

inline cell *cdr(const cell *c) {
  if (c == NULL)
    // (cdr NIL)
    return NULL;
#if CHECKS
  if (atom(c))
    pi_error(LISP_ERROR, "cdr applied to an atom");
#endif
  return c->cdr;
}

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