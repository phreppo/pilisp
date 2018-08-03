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
#include "pistack.h"
#include <stdio.h>

// ==================== Builtin Lambdas Structure ====================
cell BUILTIN_LAMBDAS[N_BUILTIN_LAMBDA];
size_t builtin_lambdas_index; // first free cell

// ==================== Builtin Macro Structure ====================
cell BUILTIN_MACROS[N_BUILTIN_MACRO];
size_t builtin_macros_index; // first free cell

/********************************************************************************
 *                                 Init Functions
 ********************************************************************************/

/**
 * @brief Initializes the variables and the structures that pilisp needs in
 * order to properly work. Always call this function before using pilisp core
 * functions
 *
 */
void init_pi(); // always call this before using pilisp

/**
 * @brief Initializes the array of builtin macros
 * 
 */
void init_builtin_macros();

/**
 * @brief Initializes the array of builtin lambdas
 * 
 */
void init_builtin_lambdas();

/**
 * @brief Initializes the variables to handle the stack
 * 
 */
void init_stack();


/**
 * @brief Provides some non-builtin definitions, like 1+
 * 
 */
void init_env(); // inits the global env

/**
 * @brief Get the compiler source hardcoded
 * 
 * @return char* string representing the compiler source code
 */
char *get_compiler_source_hardcoded();


/********************************************************************************
 *                                 Free Functions
 ********************************************************************************/

void free_pi();
void free_builtin_symbols();

/********************************************************************************
 *                                 Builtin Symbols
 ********************************************************************************/

// ==================== Lambdas ====================
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

// ==================== Keyword Symbols ====================
cell *symbol_string;

// ==================== Macros ====================
cell *symbol_setq;
cell *symbol_let;
cell *symbol_timer;
cell *symbol_defun;
cell *symbol_map;
cell *symbol_cond;
cell *symbol_quote;
cell *symbol_asm;
cell *symbol_compile;
cell *symbol_concatenate;

#endif // !PIINIT_H
       /*@}*/