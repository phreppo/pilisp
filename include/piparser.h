/** @defgroup piparser
 *
 * @brief provides lexer and parser
 *
 */

/** @addtogroup piparser */
/*@{*/

#ifndef PIPARSER
#define PIPARSER
#include <stdio.h>
#include <stdbool.h>
#include "pilisp.h"

/**
 * @brief enum for identify the type of a token
 *
 */
enum {
  TOK_NONE,
  TOK_OPEN,
  TOK_CLOSE,
  TOK_DOT,
  TOK_QUOTE, // TODO not handled
  TOK_SYM,
  TOK_NUM,
  TOK_STR
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

cell *read_sexpr(FILE *f);

cell *read_sexpr_tok(FILE *f, int tok);

#endif // !PIPARSER
/*@}*/