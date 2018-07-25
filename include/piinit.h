/** @defgroup piinit
 *
 * @brief Provides methods that have to be called before using pilisp
 *
 */

/** @addtogroup piinit */
/*@{*/

#ifndef PIINIT_H
#define PIINIT_H
#include "picell.h"
#include "piparser.h"
#include "pisettings.h"
#include <stdio.h>

// ==================== BUILTIN LAMBDAS STRUCTURE ====================
cell BUILTIN_LAMBDAS[N_BUILTIN_LAMBDA];
size_t builtin_lambdas_index; // first free cell

// ==================== BUILTIN MACRO STRUCTURE ====================
cell BUILTIN_MACROS[N_BUILTIN_MACRO];
size_t builtin_macros_index; // first free cell

/********************************************************************************
 *                                 INIT FUNCTIONS
 ********************************************************************************/

void init_pi(); // always call this before using pilisp
void init_builtin_macros();
void init_builtin_lambdas();
void init_stack();
void init_env(); // inits the global env
void init_symbols();

/********************************************************************************
 *                                 FREE FUNCTIONS
 ********************************************************************************/

void free_pi();
void free_builtin_symbols();

/********************************************************************************
 *                                 BUILTIN SYMBOLS
 ********************************************************************************/

// ==================== LAMBDAS ====================
cell *symbol_car;
cell *symbol_cdr;
cell *symbol_cons;
cell *symbol_atom;
cell *symbol_eq;      // eq
cell *symbol_eq_math; // =
cell *symbol_true;
cell *symbol_set;
cell *symbol_addition;
cell *symbol_subtraction;
cell *symbol_multiplication;
cell *symbol_division;
cell *symbol_lambda;
cell *symbol_label;
cell *symbol_load;
cell *symbol_or;
cell *symbol_and;
cell *symbol_not;
cell *symbol_greater;
cell *symbol_greater_equal;
cell *symbol_less;
cell *symbol_less_equal;
cell *symbol_length;
cell *symbol_member;
cell *symbol_nth;
cell *symbol_file_ended;
cell *symbol_env;
cell *symbol_mem_dump;
cell *symbol_collect_garbage;
cell *symbol_dotimes;
cell *symbol_list;
cell *symbol_bye;
cell *symbol_macro;
cell *symbol_integerp;
cell *symbol_symbolp;
cell *symbol_write;
cell *symbol_subseq;
cell *symbol_reverse;
cell *symbol_concatenate;
cell *symbol_append;
cell *symbol_lasm;

// ==================== KEYWORD SYMBOLS ====================
cell *symbol_string;

// ==================== MACROS ====================
cell *symbol_setq;
cell *symbol_let;
cell *symbol_timer;
cell *symbol_defun;
cell *symbol_map;
cell *symbol_cond;
cell *symbol_quote;
cell *symbol_asm;
cell *symbol_compile;
cell *symbol_compile_all;
cell *symbol_concatenate;


#endif // !PIINIT_H
/*@}*/