#include "pilisp.h"

int prompt() {
  while (1) {
    printf("%s", PROMPT_STRING);
    int token = next_token(stdin); // note: int, not char, required to handle EOF
    while (1) {                   // standard C I/O file reading loop
      print_token(token);
      token = next_token(stdin);
    }
  }
  return 0;
}

int next_token(FILE *f) {
  int token = -1;
  char c = next_char(f);
  if (c == '(')
    token = TOK_OPEN;
  else if (c == ')')
    token = TOK_CLOSE;
  else if (c == '.')
    token = TOK_DOT;
  else if (c == '\'')
    token = TOK_QUOTE;
  else {
    token = TOK_SYM;
    int i = 0;
    token_text[i++] = c;

    if (token_text[0] == '\"') {
      // the token is a string: has to be parsed in a different way: now we
      // can't skip spaces
      token = TOK_STR;
      do {
        c = (char)fgetc(f);
        if (!char_is_str_terminal(c))
          token_text[i++] = c;
        else {
          token_text[i++] = c;
          token_text[i] = '\0';
        }
      } while (!char_is_str_terminal(c));
    } else {
      // is not a string: we suppose it is a symbol
      do {
        c = (char)fgetc(f);
        if (!char_is_sym_terminal(c))
          token_text[i++] = c;
        else {
          token_text[i] = '\0';
          ungetc(
              c,
              f); // resets because he could have read something like 'symbol)'
        }
      } while (!char_is_sym_terminal(c));

      if (token_text[0] == '\"' && token_text[i - 1] == '\"') {
        // is a string
        token = TOK_STR;
      } else {
        // could be a number
        char *e;
        token_value = strtol(token_text, &e, 0);
        if (*e == '\0')
          // is effectively a number
          token = TOK_NUM;
      }
    }
  }
  return token;
}

char next_char(FILE *f) {
  char c;
  do {
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

void print_token(int tok) {
  switch (tok) {
  case TOK_NONE:
    printf("<NILL>\t\t");
    puts("NILL");
    break;
  case TOK_OPEN:
    printf("<OPEN_PAR>\t");
    puts("(");
    break;
  case TOK_CLOSE:
    printf("<CLOSE_PAR>\t");
    puts(")");
    break;
  case TOK_DOT:
    printf("<DOT>\t\t");
    puts(".");
    break;
  case TOK_QUOTE:
    printf("<QUOTE>\t\t");
    puts("\'");
    break;
  case TOK_SYM:
    printf("<SYMBOL>\t");
    puts(token_text);
    break;
  case TOK_STR:
    printf("<STRING>\t");
    puts(token_text);
    break;
  case TOK_NUM:
    printf("<NUMBER>\t");
    puts(token_text);
    break;
  default:
    // error
    break;
  }
}

bool char_is_sym_terminal(char c) {
  return c == ')' || c == ' ' || c == '\n';
}

bool char_is_str_terminal(char c) { return c == '\"'; }
