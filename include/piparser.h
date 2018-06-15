/** @defgroup piparser
 *
 * @brief provides lexer and parser
 *
 */

/** @addtogroup piparser */
/*@{*/

#ifndef PIPARSER_H
#define PIPARSER_H
#include <stdio.h>
#include <stdbool.h>
#include "pilisp.h"

/**
 * @brief enum for identify the type of a token
 *
 */
enum {
  TOK_NONE,       ///< empty file token                           
  TOK_OPEN,       ///< open par token
  TOK_CLOSE,      ///< closed par token
  TOK_DOT,        ///< dot token

  // TODO not handled      
  TOK_QUOTE,      ///< quote token
  TOK_SYM,        ///< symbol token
  TOK_NUM,        ///< number token
  TOK_STR         ///< string token
};

/**
 * @brief defines the max number of chars for a token
 *
 */
#define MAX_TOK_LEN 1024


/**
 * @brief reads the next sexpression on the file, consuming chars. Initializaes the call of the recursive version
 * 
 * @param f source file
 * @return cell* pointer to the root fo the next sexpression in the file
 */
cell *read_sexpr(FILE *f);

/**
 * @brief reads the next sexpression on the file, consuming chars. Don't use this versione unless you need to debug
 * 
 * @param f the source file
 * @param tok ridden token
 * @return cell* pointer to the root fo the next sexpression in the file
 */
cell *read_sexpr_tok(FILE *f, int tok);

/**
 * @brief Returns the text of the last ridden token
 * 
 * @return const char* 
 */
const char * get_token_text();

#endif // !PIPARSER_H
/*@}*/