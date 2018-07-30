/** @defgroup pichecks
 *
 *  @brief Provides checks shortcuts for builtin functions
 *
 */
/** @addtogroup pibuiltin */
/*@{*/
#ifndef PICHECKS_H
#define PICHECKS_H
#include "picell.h"
#include "pierror.h"

void check_append(cell *args);
void check_concatenate(cell *args);

// ==================== Arithmetic ====================
void check_addition_atom(cell *arg);
void check_subtraction(cell *args);
void check_subtraction_atom(cell *arg);
void check_multiplication_atom(cell *arg);
void check_division(cell *args);
void check_division_atom(cell *arg);

// ==================== Comparison ====================
void check_comparables(cell *args);

// ==================== Lists ====================
void check_length(cell *args);
void check_member(cell *args);
void check_nth(cell *args);
void check_subseq(cell *args);

// ==================== Utility ====================
void check_set(cell *args);

// ==================== Macros ====================
void check_setq(cell *args);

// ==================== Pilisp special functions ====================
void check_compile(cell *args);

// ==================== Basic Lisp functions ====================
void check_car(cell *args);
void check_cdr(cell *args);

#endif // !PICHECKS_H
       /*@}*/