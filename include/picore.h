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

/**
 * @brief Evaluates one sexpression and returns the result
 *
 * @param expression the sexpression to be evaluated
 * @param env list of pairs symbol - value
 * @return cell* the expression evaluated in the environment
 */
cell *eval(cell *expression, cell *env);

/**
 * @brief Applyes the function to the args in the environment
 *
 * @param fn sexpression representing the function. Can be non-atomic
 * @param args list of arguments
 * @param env list of pairs symbol - value
 * @param eval_args true if the arguments should be evaluated. They should not
 * be evaluated only when apllying a macro
 * @return cell* the result of the function applied to the arguments
 */
cell *apply(cell *fn, cell *args, cell *env, bool eval_args);

/**
 * @brief Creates a new list of pairs symbol-values starting from another. It's
 * used to extend one environment
 *
 * @param symbols_list list containing the symbols
 * @param values_list list containing the values. the order Matters: the first
 * will be matched with the first symbol the second with the second symbol and
 * so on
 * @param env the environment to be extended
 * @return cell* the new environment
 */
cell *pairlis(cell *symbols_list, cell *values_list, cell *env);

/**
 * @brief Associates one symbol to one values in the environment
 *
 * @param symbol the symbol to be associated
 * @param env the environment where the value of the symbol is contained
 * @return cell* the value of the symbol in the environment
 */
cell *assoc(cell *symbol, cell *env);

/**
 * @brief Evaluates the cars of the list in the environment. It's called when a
 * lambda is evaluated: first the list of the arguments of the lambda will be
 * evaluated with this function
 *
 * @param args the list of the arguments
 * @param env the environment
 * @return cell* the list containing the valued cars
 */
cell *evlis(cell *args, cell *env);

/**
 * @brief Evaluates the cond-special form
 *
 * @param args the list of the arguments: if the caar valuates to non-nill value
 * the the value of the cond will be the evaluated cadar
 *
 * @param env list of pairs symbol - value
 * @return cell* result of the cond
 */
cell *evcon(cell *args, cell *env);

// ==================== Support functions ====================
cell *eval_atom(cell *expression, cell *env);
cell *eval_atom_function(cell *expression, cell *env);
cell *eval_composed_function(cell *expression, cell *env);
cell *eval_macro(cell *expression, cell *env);

cell *apply_atom_function(cell *fn, cell *args, cell *env, bool eval_args);
cell *apply_composed_function(cell *fn, cell *args, cell *env, bool eval_args);
cell *apply_lambda(cell *fn, cell *args, cell *env, bool eval_args);
cell *apply_lasm(cell *fn, cell *args, cell *env, bool eval_args);
cell *apply_label(cell *fn, cell *args, cell *env, bool eval_args);
cell *apply_macro(cell *fn, cell *args, cell *env, bool eval_args);
cell *eval_lambda_and_apply(cell *fn, cell *args, cell *env, bool eval_args);

#endif // !PICORE_H
       /*@}*/