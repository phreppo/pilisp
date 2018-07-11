/** @defgroup pitestutils
 *
 * @brief Provides tools like prompts to test some functions of pilisp
 *
 */

/** @addtogroup pitestutils */
/*@{*/

#ifndef PTESTUTILS_H
#define PTESTUTILS_H
#include "pilisp.h"
#include <setjmp.h>
#include <stdio.h>

/********************************************************************************
 *                                    PROMPTS
 ********************************************************************************/

void lexer_prompt();
void parse_prompt();
int lexer_file(FILE *f);
void pairlis_prompt();
void eval_prompt();
void mem_prompt();

#endif // !PTESTUTILS_H
       /*}*/