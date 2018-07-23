/** @defgroup piprint
 *
 * @brief Handles printing messages and data structures
 *
 */

/** @addtogroup pilisp */
/*@{*/
#ifndef PIPRINT_H
#define PIPRINT_H
#include "pilisp.h"

/********************************************************************************
 *                                PRINT FUNCTIONS
 ********************************************************************************/

enum sexpr_print_mode {
  SEXPR_PRINT_DEFAULT, ///< default print mode
  SEXPR_PRINT_VERBOSE  ///< verbose print mode: (a b c) is printed (a . (b . (c
                       ///< .NIL)))
};

void print_sexpr_mode(const cell *c, unsigned char mode);
void print_sexpr(const cell *c);
void print_token(int tok);
void print_cell_block(const cell_block *block);
void print_cell(const cell *cell);
void print_cell_space(const cell_space *cs);
void print_free_cells(const cell_space *cs);
void print_global_env(const cell *env);
void pi_message(const char *);
void print_stack();

#endif // !PIPRINT_H
       /*@}*/