#include "piparser.h"

int next_token(FILE *f) {
  int token = -1;
  char c = next_char(f);
  if (c == 0 || feof(f))
    return TOK_NONE;
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
        if (feof(f) || !char_is_sym_terminal(c))
          token_text[i++] = c;
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

char next_char(FILE *f) {
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
    // something else?
    break;
  }
}

bool char_is_sym_terminal(char c) {
  return c == '(' || c == ')' || c == ' ' || c == '\n' || c == 0 || c == -1 ||
         c == '.';
}

bool char_is_str_terminal(char c) { return c == '\"'; }

bool token_text_is_nill() {
  char *nillstr = "NILL";
  int i = 0;
  for (i = 0; i < 3; i++) {
    if (token_text[i] != nillstr[i])
      return false;
  }
  return true;
}

cell *read_sexpr(FILE *f) {
  int tok = next_token(f);
  if (tok != TOK_NONE)
    return read_sexpr_tok(f, tok);
  return 0;
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
    c = (token_text_is_nill() ? 0 : mk_sym(token_text));
    break;
  case TOK_CLOSE:
    pi_error(LISP_ERROR, "unexpected )");
    break;
  // TODO: implement quote parsing
  // case TOK_QUOTE:
  //   tok=next_token(f);
  //   return mk_cons(quote_atom,mk_cons(read_sexpr_tok(f,tok),0));
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

        // head of the cdr: we're going to build the tree
        cell *cdr_head = read_sexpr_tok(f, tok);
        cdr = mk_cons(cdr_head, NULL);

        tok = next_token(f);
        // this keeps track of the last cdr added, because we need to attach
        // list members to the end of the last cdr. Example: (a b c d) needs to
        // keep track of the last to create the nested structure (a . (b . (c .
        // (d . NILL))))
        cell *last_cdr = cdr;
        while (tok != TOK_CLOSE) {
          // create a new level
          cell *new_cdr = mk_cons(read_sexpr_tok(f, tok), NULL);
          // update cycle variables
          last_cdr->cdr = new_cdr;
          last_cdr = new_cdr;
          tok = next_token(f);
        }
      }
      // create the cell
      c = mk_cons(car, cdr);
    }
    break;
  case TOK_DOT:
    pi_error(LISP_ERROR, "unexpected .");
    break;
  default:
    // error ?
    break;
  };
  return c;
}

void print_sexpr(const cell *c) {
  if (c) {

    switch (c->type) {

    case TYPE_NUM:
      printf("%i", c->value);
      break;
    case TYPE_STR:
      printf("%s", c->str);
      break;
    case TYPE_SYM:
      printf("%s", c->sym);
      break;
    case TYPE_CONS:
      if (c->cdr == NULL) {
        print_sexpr(c->car);
      } else {
        printf("(");
        print_sexpr(c->car);
        printf(" . ");
        print_sexpr(c->cdr);
        printf(")");
      }
      break;
    default:
      break;
    }
  } else {
    // empty cell
    printf("NIL");
  }
}