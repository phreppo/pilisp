/** @defgroup pierror
 *
 * @brief Provides errors handling
 *
 */

/** @addtogroup pierror */
/*@{*/
#ifndef PERROR_H
#define PERROR_H
#include "pilisp.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

enum error_types {
  NO_ERROR = -1,    ///< no error occurrred
  LISP_ERROR = 1,   ///< LISP syntax error
  MEMORY_ERROR = 2, ///< memory error
  MODE_ERROR = 3    ///< error passing mode to some kind of functions
};

// ==================== ERRORS THROWING ====================
void pi_error(int CODE, char *message);
void pi_lisp_error(char *message); // throws LISP_ERROR
void pi_error_few_args();          // throws "too few args"
void pi_error_many_args();         // throws "too many args"

// ==================== CHECKS ====================

// checks that the list args has exaclty 1 args
void check_one_arg(const cell *args);
// checks that the list args has exaclty 2 args
void check_two_args(const cell *args);
// checks that the list args has exaclty 3 args
void check_three_args(const cell *args);

// ==================== LAST ERROR ====================
int get_last_error();
bool had_error();
void reset_error();

#endif // !PERROR_H
       /*@}*/