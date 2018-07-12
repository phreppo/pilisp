/** @defgroup piparser
 *
 * @brief Provides lexer and parser
 *
 */

/** @addtogroup piparser */
/*@{*/
#ifndef PIPARSER_H
#define PIPARSER_H
#include "pilisp.h"
#include <stdbool.h>
#include <stdio.h>

/********************************************************************************
*                               PARSER AND LEXER
********************************************************************************/

enum {
  TOK_NONE,  ///< empty file token
  TOK_OPEN,  ///< open par token
  TOK_CLOSE, ///< closed par token
  TOK_DOT,   ///< dot token
  TOK_QUOTE, ///< quote token
  TOK_SYM,   ///< symbol token
  TOK_NUM,   ///< number token
  TOK_STR    ///< string token
};

cell *read_sexpr(FILE *f);
cell *read_sexpr_tok(FILE *f, int tok);
const char *get_token_text();
int next_token(FILE *f);

#endif // !PIPARSER_H
       /*@}*/