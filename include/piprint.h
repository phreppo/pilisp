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

void pi_message(const char *);

/**
 * @brief prints in stdout one formatted token. The text of the token is stored
 * inside the variable token_text
 *
 * @param tok the token
 */
void print_token(int tok);

/**
 * @brief enumeration for identify print mode
 *
 */
enum {
  SEXPR_PRINT_DEFAULT, ///< default print mode
  SEXPR_PRINT_VERBOSE  ///< verbose print mode: (a b c) is printed (a . (b . (c .NIL)))
};

/**
 * @brief prints the sexpression pointed from c
 * 
 * @param c the sexpression that has to be printed
 */
void print_sexpr(const cell *c);

/**
 * @brief prints the sexpression pointed from c in SEXPR_PRINT_DEFAULT or SEXPR_PRINT_VERBOSE
 * 
 * @param c the sexpression that has to be printed
 * @param mode SEXPR_PRINT_DEFAULT or SEXPR_PRINT_VERBOSE
 */
void print_sexpr_mode(const cell *c, unsigned char mode);

void print_cell_block(const cell_block * block);

void print_cell(const cell * cell);

void print_cell_space(const cell_space * cs);

#endif // !PIPRINT_H

/*@}*/
