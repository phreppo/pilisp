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
#include <error.h>
#include <errno.h>

/**
 * @brief Displays pilisp prompt
 * 
 * @return int 0 if no error occurred
 */
int prompt();

/**
 * @brief Interprets the LISP file
 * 
 * @param file_name The name of the file
 * @return int 0 if no error occurred
 */
int interpret_file(char* file_name);

#endif // !PILISP

/*@}*/
