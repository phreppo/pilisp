/** @defgroup Pilisp
 *
 * This module does yada yada yada
 *
 */

/** @addtogroup Pilisp */
/*@{*/
#ifndef PILISP
#define PILISP
#define PROMPT_STRING "pi>"
#include <ctype.h>
#include <errno.h>
#include <error.h>
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pierror.h"
#include "picell.h"
#include "piparser.h"

/**
 * @brief Displays pilisp prompt
 *
 * @return int 0 if no error occurred
 */
int prompt();

#endif // !PILISP

/*@}*/
