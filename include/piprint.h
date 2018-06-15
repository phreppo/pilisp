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

enum { SEXPR_PRINT_DEFAULT, SEXPR_PRINT_VERBOSE };

void print_sexpr(const cell *c);

void print_sexpr_mode(const cell *c, unsigned char mode);

static bool cell_was_printed(const cell *c,const cell **printed_cons_cells,
                             unsigned long level);

static void print_sexpr_rec_dot(const cell *c,const cell **printed_cons_cells,
                                unsigned long level);

static void print_sexpr_rec_list(const cell *c,const cell **printed_cons_cells,
                          unsigned long level);

#endif // !PIPRINT_H

/*@}*/
