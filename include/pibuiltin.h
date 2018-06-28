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
#include <stdbool.h>
#include <time.h>

// BASIC
cell *car(const cell *c);
cell *cdr(const cell *c);
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

// ==================== LOGIC ==================== 
cell * or (const cell *operands);
cell * and (const cell *operands);
cell * not(const cell *operands);

// ==================== COMPARISON ====================
cell *greater(const cell *operands);
cell *greater_eq(const cell *operands);
cell *less(const cell *operands);
cell *less_eq(const cell *operands);

// ==================== ARITHMETIC ====================
cell *addition(const cell *numbers);
cell *subtraction(const cell *numbers);
cell *multiplication(const cell *numbers);
cell *division(const cell *numbers);

// ==================== UTILITY ====================
cell *timer(cell *arg, cell **env);
cell *set(cell *args);
cell *load(cell *arg, cell **env);

// ==================== LISTS ====================
cell *length(const cell *list);
cell *member(const cell *list);
cell *nth(const cell *list);

#endif // !PIBUILTIN_H

/*@}*/