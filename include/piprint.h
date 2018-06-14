/** @defgroup piprint
 *
 * @brief Handles printing messages and data structures
 *
 */

/** @addtogroup pilisp */
/*@{*/
#ifndef PIPRINT
#define PIPRINT
#include "pilisp.h"

void pi_message(const char *);

/**
 * @brief prints in stdout one formatted token. The text of the token is stored
 * inside the variable token_text
 *
 * @param tok the token
 */
void print_token(int tok);

void print_sexpr(const cell *c);

static bool cell_was_printed(const cell *c, cell **printed_cons_cells,
                             unsigned long level);

static void print_sexpr_rec(const cell *c, cell **printed_cons_cells,
                            unsigned long level);

#endif // !PIPRINT

/*@}*/
