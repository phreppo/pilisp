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
cell *assoc(cell *x, cell *l);
cell *evlis(cell *m, cell *a);
cell *evcon(cell *c, cell *a);

// ==================== Support functions ====================
cell *eval_atom(cell * expression, cell * env);
cell *eval_atom_function(cell * expression, cell * env);
cell *eval_composed_function(cell * expression, cell * env);
cell *eval_macro(cell * expression, cell * env);

cell *apply_atom_function(cell *fn, cell *args, cell *env, bool eval_args);
cell *apply_composed_function(cell *fn, cell *args, cell *env, bool eval_args);
cell *apply_lambda(cell *fn, cell *args, cell *env, bool eval_args);
cell *apply_lasm(cell *fn, cell *args, cell *env, bool eval_args);
cell *apply_label(cell *fn, cell *args, cell *env, bool eval_args);
cell *apply_macro(cell *fn, cell *args, cell *env, bool eval_args);
cell *eval_lambda_and_apply(cell *fn, cell *args, cell *env, bool eval_args);

#endif // !PICORE_H
       /*@}*/