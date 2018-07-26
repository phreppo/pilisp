#include "piparser.h"

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
 * @brief returns the next char in the input source. Skips the comments
 *
 * @param f the input stream
 * @return char the next char
 */
static char next_char(FILE *f) {
  char c;
  do {
    if (!f || feof(f))
      return 0;
    int ch = fgetc(f);
    if (ch == EOF)
      return 0;
    c = (char)ch;
    if (c == ';') { // single-line comment
      do {
        ch = fgetc(f);
        if (ch == EOF)
          return 0;
      } while ((char)ch != '\n');
      c = (char)ch;
    }
  } while (isspace(c));
  return c;
}

/**
 * @brief returns true if a char can terminate a symbol (e.g. ), (space), \n)
 *
 * @param c the input char
 * @return true can terminate a symbol
 * @return false otherwise
 */
static bool char_is_sym_terminal(char c) {
  return c == '(' || c == ')' || c == ' ' || c == '\n' || c == 0 || c == -1 ||
         c == '.' || c == EOF;
}

/**
 * @brief returs true if a char terminates a strings (e.g. ")
 *
 * @param c the input char
 * @return true c can terminate a string
 * @return false otherwise
 */
static bool char_is_str_terminal(char c) { return c == '\"'; }

/**
 * @brief checks if the token text equals "NILL"
 *
 * @return true the token text equals "NILL"
 * @return false otherwise
 */
static bool token_text_is_nil() {
  char *nillstr = "NIL";
  int i = 0;
  for (i = 0; i < 3; i++) {
    if (token_text[i] != nillstr[i])
      return false;
  }
  return true;
}

/**
 * @brief reads and returns the identifier of the next token in f
 *
 * @param f the input source
 * @return int the code of the token
 */
int next_token(FILE *f) {
  int token = -1;
  char c = next_char(f);
  if (c == 0 || feof(f)) {
    token_text[0] = '\0';
    return TOK_NONE;
  }
  if (c == '(') {
    token_text[0] = '\0';
    token = TOK_OPEN;
  } else if (c == ')') {
    token_text[0] = '\0';
    token = TOK_CLOSE;
  } else if (c == '.') {
    token_text[0] = '\0';
    token = TOK_DOT;
  } else if (c == '\'') {
    token_text[0] = '\0';
    token = TOK_QUOTE;
  } else {
    token = TOK_SYM;
    int i = 0;
    token_text[i++] = c;

    if (token_text[0] == '\"') {
      // the token is a string: has to be parsed in a different way: now we
      // can't skip spaces
      token = TOK_STR;
      i--;
      do {
        c = (char)fgetc(f);
        if (!char_is_str_terminal(c))
          token_text[i++] = c;
        else {
          token_text[i] = '\0';
        }
      } while (!char_is_str_terminal(c));
    } else {
      // is not a string: we suppose it is a symbol
      do {
        token_text[0] = toupper(token_text[0]);
        c = (char)fgetc(f);
        if (!char_is_sym_terminal(c))
          token_text[i++] = toupper(c);
        else {
          token_text[i] = '\0';
          ungetc(
              c,
              f); // resets because he could have read something like 'symbol)'
        }
      } while (!char_is_sym_terminal(c));
      // could be a number
      char *e;
      token_value = strtol(token_text, &e, 0);
      if (*e == '\0')
        // is effectively a number
        token = TOK_NUM;
    }
  }
  return token;
}

cell *read_sexpr(FILE *f) {
  int tok = next_token(f);
  if (tok != TOK_NONE)
    return read_sexpr_tok(f, tok);
  return symbol_file_ended;
}

cell *read_sexpr_tok(FILE *f, int tok) {
  cell *c = 0;
  switch (tok) {
  case TOK_NUM:
    c = mk_num(token_value);
    break;
  case TOK_STR:
    c = mk_str(token_text);
    break;
  case TOK_SYM:
    c = (token_text_is_nil() ? 0
                             : mk_sym(token_text)); // can be a builtin lambda
    break;
  case TOK_CLOSE:
    pi_error(LISP_ERROR, "unexpected )");
  case TOK_QUOTE:
    tok = next_token(f);
    return mk_cons(mk_sym("QUOTE"), mk_cons(read_sexpr_tok(f, tok), 0));
  case TOK_OPEN:
    tok = next_token(f);
    if (tok == TOK_CLOSE)
      // () cell
      c = NULL;
    else {
      // read car
      cell *car = read_sexpr_tok(f, tok);
      cell *cdr = NULL;
      // read after head: we can have . || sexpr
      tok = next_token(f);
      if (tok == TOK_DOT) {
        // uses the dot notation
        tok = next_token(f);
        // read cdr
        cdr = read_sexpr_tok(f, tok);
        tok = next_token(f);
        if (tok != TOK_CLOSE)
          pi_error(LISP_ERROR, ") expected");
      } else if (tok == TOK_CLOSE) {
        // found something like (a) = (a . NILL)
        // nothing to do: cdr=NULL is ok
      } else {
        // you are here: ( [car] [something that is not a dot or a ')', aka a
        // sexpr] ....

        cell *cdr_head = read_sexpr_tok(f, tok);
        cdr = mk_cons(cdr_head, NULL);

        tok = next_token(f);
        // this keeps track of the last cdr added, because we need to attach
        // list members to the end of the last cdr. Example: (a b c d) needs
        // to keep track of the last to create the nested structure (a . (b .
        // (c . (d . NILL))))
        cell *last_cdr = cdr;
        while (tok != TOK_CLOSE) {

          if (tok == TOK_DOT) {
            // something like: ( {list} . => we have to read the last atom and
            // read a close
            cell *last_sexpr = read_sexpr(f);
            last_cdr->cdr = last_sexpr;
            tok = next_token(f);
          } else {
            // create a new level
            cell *new_cdr = mk_cons(read_sexpr_tok(f, tok), NULL);
            // update cycle variables
            last_cdr->cdr = new_cdr;
            last_cdr = new_cdr;
            tok = next_token(f);
          }

        }
      }
      // create the cell
      c = mk_cons(car, cdr);
    }
    break;
  case TOK_DOT:
    pi_error(LISP_ERROR, "unexpected .");
  default:
    // error ?
    break;
  };
  return c;
}

char *get_token_text() { return token_text; }