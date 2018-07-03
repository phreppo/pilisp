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
#include "pisettings.h"
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

// ==================== VARIABLES FOR HANDLING ERRORS ====================
int jmp_destination;
jmp_buf env_buf;

/********************************************************************************
 *                           BASIC INTERPRETER FUNCTIONS
 ********************************************************************************/

int pi_prompt();
cell * parse_file(char * file_path);

#endif // !PILISP_h

/*@}*/
