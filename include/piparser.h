#ifndef PIPARSER
#define PIPARSER
#include <stdio.h>
#include <stdbool.h>
#include "pilisp.h"

/**
 * @brief Enum for identify the type of a token
 *
 */
enum {
  TOK_NONE,
  TOK_OPEN,
  TOK_CLOSE,
  TOK_DOT,
  TOK_QUOTE,
  TOK_SYM,
  TOK_NUM,
  TOK_STR
};
/**
 * @brief Defines the max number of chars for a token
 *
 */
#define MAX_TOK_LEN 1024

/**
 * @brief text of a token
 *
 */
char token_text[MAX_TOK_LEN];

/**
 * @brief Value for numeric tokens
 *
 */
long token_value;


/**
 * @brief Reads and returns the identifier of the next token in f
 *
 * @param f the input source
 * @return int the code of the token
 */
int next_token(FILE *f);

/**
 * @brief Returns the next char in the input source. Skips the comments
 *
 * @param f the input stream
 * @return char the next char
 */
char next_char(FILE *f);

/**
 * @brief Prints in stdout one formatted token. The text of the token is stored
 * inside the variable token_text
 *
 * @param tok the token
 */
void print_token(int tok);

/**
 * @brief Returns true if a char can terminate a symbol (e.g. ), (space), \n)
 *
 * @param c the input char
 * @return true can terminate a symbol
 * @return false otherwise
 */
bool char_is_sym_terminal(char c);

/**
 * @brief Returs true if a char terminates a strings (e.g. ")
 *
 * @param c the input char
 * @return true c can terminate a string
 * @return false otherwise
 */
bool char_is_str_terminal(char c);

/**
 * @brief checks if the token text equals "NILL"
 *
 * @return true the token text equals "NILL"
 * @return false otherwise
 */
bool token_text_is_nill();

cell *read_sexpr(FILE *f);

cell *read_sexpr_tok(FILE *f, int tok);

void print_sexpr(const cell *c);

#endif // !PIPARSER