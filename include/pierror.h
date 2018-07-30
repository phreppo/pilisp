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

// ==================== Errors Throwing ====================
void pi_error(int CODE, char *message);
void pi_lisp_error(char *message); // throws LISP_ERROR
void pi_error_few_args();          // throws "too few args"
void pi_error_many_args();         // throws "too many args"
void pi_error_stack();             
void pi_error_stack_overflow();
void pi_error_stack_undeflow();

// ==================== Number of arguments checks ====================
void check_zero_arg(cell * args);
void check_one_arg(cell *args);
void check_two_args(cell *args);
void check_three_args(cell *args);

// ==================== Last error informations ====================
int get_last_error();
bool had_error();
void reset_error();

#endif // !PERROR_H
       /*@}*/