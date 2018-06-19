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

cell *symbol_car;
cell *symbol_cdr;
cell *symbol_cons;
cell *symbol_atom;
cell *symbol_eq;
cell *symbol_true;
cell *symbol_plus;
cell *symbol_minus;
cell *symbol_multiplication;
cell *symbol_division;

void init_env();

int jmp_destination;
jmp_buf env_buf;

/**
 * @brief Displays pilisp prompt
 *
 * @return int 0 if no error occurred
 */
int prompt();

#endif // !PILISP_h

/*@}*/
