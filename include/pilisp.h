/** @defgroup pilisp
 *
 * @brief Links the other modules of Pilisp
 *
 */

/** @addtogroup pilisp */
/*@{*/

#ifndef PILISP_h
#define PILISP_h
#define PROMPT_STRING "pi>"
#include "pibuiltin.h"
#include "picell.h"
#include "picore.h"
#include "pierror.h"
#include "pifile.h"
#include "piinit.h"
#include "piparser.h"
#include "piprint.h"
#include "pisettings.h"
#include <ctype.h>
#include <errno.h>
#include <error.h>
#include <setjmp.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/********************************************************************************
*                            VARIABLES FOR HANDLING ERRORS
********************************************************************************/
int jmp_destination;
jmp_buf env_buf;

/********************************************************************************
 *                           BASIC INTERPRETER FUNCTIONS
 ********************************************************************************/

int pi_prompt();
int pi_parse_args_files(int argc, char **argv);

#endif // !PILISP_h
       /*@}*/