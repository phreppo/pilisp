/** @defgroup picore
 *
 *  @brief Provides LISP core functions: eval and apply
 *
 */

/** @addtogroup picore */
/*@{*/

#ifndef PICORE_H
#define PICORE_H
#include "pilisp.h"

// ==================== Core functions ====================
cell *eval(cell *e, cell *a);
cell *apply(cell *fn, cell *x, cell *a, bool eval_args);
cell *pairlis(cell *x, cell *y, cell *a);
cell *assoc(const cell *x, cell *l);
cell *evlis(cell *m, cell *a);
cell *evcon(cell *c, cell *a);

// ==================== Support functions ====================
cell *apply_atom_function(cell *fn, cell *x, cell *a, bool eval_args);

#endif // !PICORE_H
       /*@}*/