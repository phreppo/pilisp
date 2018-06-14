/** @defgroup pilisp
 *
 * @brief Links the other modules of pilisp
 *
 */

/** @addtogroup pilisp */
/*@{*/
#ifndef PILISP
#define PILISP
#define PROMPT_STRING "pi>"
#include <ctype.h>
#include <errno.h>
#include <error.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <setjmp.h>
#include "picell.h"
#include "pierror.h"
#include "piparser.h"

int jmp_destination;
jmp_buf env_buf;

/**
 * @brief Displays pilisp prompt
 *
 * @return int 0 if no error occurred
 */
int prompt();

void pi_message(const char *);

#endif // !PILISP

/*@}*/
