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

void print_sexpr_mode(cell *c, unsigned char mode);
void print_sexpr(cell *c);
void print_sexpr_to_file(cell *c, FILE *f);
void print_token(int tok);
void print_cell_block(cell_block *block);
void print_cell(cell *cell);
void print_cell_space(cell_space *cs);
void print_free_cells(cell_space *cs);
void print_global_env(cell *env);
void pi_message(char *);
void print_stack();

#endif // !PIPRINT_H
       /*@}*/