/** @defgroup pierror
 *
 * @brief Provides errors handling
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

enum{
    NO_ERROR = -1,
    LISP_ERROR = 1,
    MEMORY_ERROR = 2,
    MODE_ERROR = 3
};

static int last_error=NO_ERROR;

void pi_error(int CODE, const char* message);

int get_last_error();

bool had_error();

void reset_error();

#endif // !PERROR
/*@}*/