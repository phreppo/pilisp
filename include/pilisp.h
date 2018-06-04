/** @defgroup Pilisp
 *
 * This module does yada yada yada
 *
 */

/** @addtogroup Pilisp */
/*@{*/
#ifndef PILISP
#define PILISP
#define PROMPT_STRING "pi> "
#define MAX_LINE_SIZE 1024
#include <math.h>
#include <stdlib.h>
#include <stdio.h>

int prompt();

int parse_file(char* file_name);

#endif // !PILISP

/*@}*/
