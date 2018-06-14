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
 * @brief text of a token
 *
 */
static char token_text[MAX_TOK_LEN];

/**
 * @brief value for numeric tokens
 *
 */
static long token_value;

/**
 * @brief reads and returns the identifier of the next token in f
 *
 * @param f the input source
 * @return int the code of the token
 */
int next_token(FILE *f);

/**
 * @brief returns the next char in the input source. Skips the comments
 *
 * @param f the input stream
 * @return char the next char
 */
static char next_char(FILE *f);

/**
 * @brief returns true if a char can terminate a symbol (e.g. ), (space), \n)
 *
 * @param c the input char
 * @return true can terminate a symbol
 * @return false otherwise
 */
static bool char_is_sym_terminal(char c);

/**
 * @brief returs true if a char terminates a strings (e.g. ")
 *
 * @param c the input char
 * @return true c can terminate a string
 * @return false otherwise
 */
static bool char_is_str_terminal(char c);

/**
 * @brief checks if the token text equals "NILL"
 *
 * @return true the token text equals "NILL"
 * @return false otherwise
 */
static bool token_text_is_nill();

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