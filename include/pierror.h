/** @defgroup pierror
 *
 * This module proves error handling
 *
 */

/** @addtogroup pierror */
/*@{*/
#ifndef PERROR
#define PERROR
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pilisp.h"

#define LISP_ERROR 1

void pi_error(int CODE, const char* message);

#endif // !PERROR
/*@}*/