/** @defgroup pilisp
 *
 * @brief Links the other modules of pilisp
 *
 */

/** @addtogroup pilisp */
/*@{*/
#ifndef PILISP_h
#define PILISP_h
#define PROMPT_STRING "pi>"
#include "pibuiltin.h"
#include "picell.h"
#include "piinit.h"
#include "picore.h"
#include "pierror.h"
#include "piparser.h"
#include "piprint.h"
#include <ctype.h>
#include <errno.h>
#include <error.h>
#include <setjmp.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define ANSI_COLOR_RED     "\x1b[31m"
#define ANSI_COLOR_GREEN   "\x1b[32m"
#define ANSI_COLOR_YELLOW  "\x1b[33m"
#define ANSI_COLOR_BLUE    "\x1b[34m"
#define ANSI_COLOR_MAGENTA "\x1b[35m"
#define ANSI_COLOR_CYAN    "\x1b[36m"
#define ANSI_COLOR_RESET   "\x1b[0m"

int jmp_destination;
jmp_buf env_buf;

/**
 * @brief Displays pilisp prompt
 *
 * @return int 0 if no error occurred
 */
int pi_prompt();

#endif // !PILISP_h

/*@}*/
