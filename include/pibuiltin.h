/** @defgroup pibuiltin
 *
 *  @brief Provides builtin lambdas: for example car, cdr
 *
 */
/** @addtogroup pibuiltin */
/*@{*/
#ifndef PIBUILTIN_H
#define PIBUILTIN_H
#include "picell.h"
#include "pilisp.h"
#include "pistack.h"
#include "piutils.h"
#include <stdbool.h>
#include <time.h>
#include <stdlib.h>
#include <math.h>

// ==================== INLINE FUNCTIONS FOR EVAL ====================
// not usable in the interpreter: no checks! => use only in the eval
#if INLINE_FUNCTIONS
inline cell *caar(cell *c) { return c->car->car; }
inline cell *cddr(cell *c) { return c->cdr->cdr; }
inline cell *cadr(cell *c) { return c->cdr->car; }
inline cell *cdar(cell *c) { return c->car->cdr; }
inline cell *cadar(cell *c) { return c->car->cdr->car; }
inline cell *caddr(cell *c) { return c->cdr->cdr->car; }
#else
cell *caar(cell *c);
cell *cddr(cell *c);
cell *cadr(cell *c);
cell *cdar(cell *c);
cell *cadar(cell *c);
cell *caddr(cell *c);
#endif

// ==================== BASIC APPLY ====================
// differences from the first basic block: these functions can be called from
// the apply, because they do cell_remove and cell_push and check for args error
cell *builtin_car(cell *args);
cell *builtin_cdr(cell *args);
cell *builtin_cons(cell *args);
cell *builtin_atom(cell *args);
cell *builtin_eq(cell *args);

// ==================== LOGIC ====================
cell * or (cell *operands);
cell * and (cell *operands);
cell * not(cell *operands);

// ==================== COMPARISON ====================
cell *greater(cell *operands);
cell *greater_eq(cell *operands);
cell *less(cell *operands);
cell *less_eq(cell *operands);
cell *integerp(cell *arg);
cell *symbolp(cell *arg);

// ==================== ARITHMETIC ====================
cell *addition(cell *numbers);
cell *subtraction(cell *numbers);
cell *multiplication(cell *numbers);
cell *division(cell *numbers);

// ==================== UTILITY ====================
cell *set(cell *args);
cell *load(cell *arg, cell *env);
cell *write(cell *arg);
cell *bye(cell *arg);
cell *mem_dump(cell *arg);
cell *env(cell *arg);
cell *collect_garbage_call(cell *arg);

// ==================== LISTS ====================
cell *length(cell *list);
cell *member(cell *list);
cell *nth(cell *list);
cell *list(cell *list);
cell *subseq(cell *list); // substr
cell *reverse(cell *list);
cell *concatenate(cell *list); // works only on strings
cell *append(cell *list);

// ==================== MACROS ====================
cell *setq(cell *args, cell *env);
cell *defun(cell *args, cell *env);
cell *let(cell *args, cell *env);
cell *map(cell *args, cell *env);
cell *quote(cell *args, cell *env);
cell *timer(cell *arg, cell *env);
cell *cond(cell *arg, cell *env);
cell *dotimes(cell *arg, cell *env);

// ==================== COMPILER FUNCTIONS ====================
cell *asm_call(cell *args, cell *env);
cell *compile(cell *c, cell *env);

// ==================== BASIC FUNCTIONS ====================
// works also on lists: eq does not, but 'it's slower
bool total_eq(cell *c1, cell *c2);

#if INLINE_FUNCTIONS
inline cell *cons(cell *car, cell *cdr) { return mk_cons(car, cdr); }

inline int atom(cell *c) {
  return (c == NULL) ||
         (c->type == TYPE_SYM || c->type == TYPE_NUM || c->type == TYPE_STR ||
          c->type == TYPE_BUILTINLAMBDA || c->type == TYPE_BUILTINMACRO ||
          c->type == TYPE_KEYWORD);
}

inline bool eq(cell *v1, cell *v2) {
  if (!v1 || !v2)
    return (v1 == v2);
  if (is_num(v1) && is_num(v2))
    return (v1->value == v2->value);
  if (is_str(v1) && is_str(v2))
    return (strcmp(v1->str, v2->str) == 0);
  return (v1 == v2);
}

// works also on lists: eq does not, but 'it's slower
bool total_eq(cell *c1, cell *c2);

inline cell *car(cell *c) {
  if (c == NULL)
    return NULL;
#if CHECKS
  if (atom(c))
    pi_lisp_error("car applied to an atom");
#endif
  return c->car;
}

inline cell *cdr(cell *c) {
  if (c == NULL)
    return NULL;
#if CHECKS
  if (atom(c))
    pi_lisp_error("cdr applied to an atom");
#endif
  return c->cdr;
}
#else

cell *cons(cell *car, cell *cdr);
int atom(cell *c);
bool eq(cell *v1, cell *v2);
cell *car(cell *c);
cell *cdr(cell *c);

#endif

#endif // !PIBUILTIN_H
       /*@}*/