#include "piprint.h"

void pi_message(const char *message) {
  printf("%s %s\n", PROMPT_STRING, message);
}

void print_token(int tok) {
  // TODO: puts(token_text) broken
  const char * token_text = get_token_text();
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

void print_sexpr(const cell * c){
    print_sexpr_mode(c,SEXPR_PRINT_DEFAULT); // default mode
}

void print_sexpr_mode(const cell *c, unsigned char mode) {
  const cell **printed_cons_cells = malloc(sizeof(cell *) * MAX_CELLS);
  unsigned long level = 0;

  switch (mode) {
  case SEXPR_PRINT_VERBOSE:
    print_sexpr_rec_dot(c, printed_cons_cells, level);
    break;
  case SEXPR_PRINT_DEFAULT:
    print_sexpr_rec_list(c, printed_cons_cells, level);
    break;
  default:
    pi_error(MODE_ERROR, "Unknown print mode");
    break;
  }
  free(printed_cons_cells);
}

static bool cell_was_printed(const cell *c,const cell **printed_cons_cells,
                             unsigned long level) {
  unsigned long i;
  // start from the end
  for (i = 0; i < level; i++)
    if (printed_cons_cells[i] == c)
      return true;
  return false;
}

void print_sexpr_rec_dot(const cell *c,const cell **printed_cons_cells,
                         unsigned long level) {
  if (c) {
    switch (c->type) {

    case TYPE_NUM:
      printf("%i", c->value);
      break;

    case TYPE_STR:
      printf("\"%s\"", c->str);
      break;

    case TYPE_SYM:
      printf("%s", c->sym);
      break;

    case TYPE_CONS:
      // could be a self referenced structure
      if (!cell_was_printed(c, printed_cons_cells, level)) {
        // mark the cell as printed
        printed_cons_cells[level++] = c;
        printf("(");
        print_sexpr_rec_dot(c->car, printed_cons_cells, level);
        printf(" . ");
        print_sexpr_rec_dot(c->cdr, printed_cons_cells, level);
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

void print_sexpr_rec_list(const cell *c,const cell **printed_cons_cells,
                          unsigned long level) {
  if (c) {
    switch (c->type) {

    case TYPE_NUM:
      printf("%i", c->value);
      break;

    case TYPE_STR:
      printf("\"%s\"", c->str);
      break;

    case TYPE_SYM:
      printf("%s", c->sym);
      break;

    case TYPE_CONS:
      // could be a self referenced structure
      if (!cell_was_printed(c, printed_cons_cells, level)) {
        // mark the cell as printed
        printed_cons_cells[level++] = c;
        if (c->cdr == NULL) {
          // only head
          printf("(");
          print_sexpr_rec_list(c->car, printed_cons_cells, level);
          printf(")");
        } else {
          // car && cdr present
          printf("(");
          // print car
          print_sexpr_rec_list(c->car, printed_cons_cells, level);
          printf(" ");
          const cell *cdr = c->cdr;
          if (cdr->type != TYPE_CONS) {
            // cdr is not a cons:
            printf(". ");
            print_sexpr_rec_list(cdr, printed_cons_cells, level);
          } else
            while (cdr) {
              // cdr is a cons
              print_sexpr_rec_list(cdr->car, printed_cons_cells, level);
              printed_cons_cells[level++] = cdr;
              cdr = cdr->cdr;
              if (cdr)
                printf(" ");
            }
          printf(")");
        }
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
