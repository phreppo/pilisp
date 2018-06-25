#include "piprint.h"

static bool cell_was_printed(const cell *c, const cell **printed_cons_cells,
                             unsigned long level) {
  unsigned long i;
  // start from the end
  for (i = 0; i < level; i++)
    if (printed_cons_cells[i] == c)
      return true;
  return false;
}

static void print_sexpr_rec_dot(const cell *c, const cell **printed_cons_cells,
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
      pi_error(MODE_ERROR, "Unknown cell type");
    }
  } else {
    // empty cell
    printf("NIL");
  }
}

static void print_sexpr_rec_list(const cell *c, const cell **printed_cons_cells,
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
      if (!cell_was_printed(c, printed_cons_cells, level)) {
        printed_cons_cells[level++] = c;
        printf("(");
        while (c->cdr && c->cdr->type == TYPE_CONS) {
          print_sexpr_rec_list(c->car, printed_cons_cells, level);
          printf(" ");
          c = c->cdr;
        }
        print_sexpr_rec_list(c->car, printed_cons_cells, level);
        if (c->cdr) {
          printf(" . ");
          print_sexpr_rec_list(c->cdr, printed_cons_cells, level);
        }
        printf(")");
      }
      break;
    default:
      pi_error(MODE_ERROR, "Unknown cell type");
    }
  } else {
    printf("NIL");
  }
}

void pi_message(const char *message) {
  printf("%s %s\n", PROMPT_STRING, message);
}

void print_token(int tok) {
  const char *token_text = get_token_text();
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
    pi_error(MODE_ERROR, "Unknown token type");
  }
}

void print_sexpr(const cell *c) {
  print_sexpr_mode(c, SEXPR_PRINT_DEFAULT); // default mode
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
  }
  free(printed_cons_cells);
}

void print_cell_block(const cell_block *block) {
  if (block) {
    size_t s = block->size;
    cell *arr = block->block;
    int i = 0;
    for (i = 0; i < s; i++) {
      printf(ANSI_COLOR_GREEN "%p\t" ANSI_COLOR_RESET,arr+i);
      print_cell(arr+i);
      puts("");
    }
  }
}

void print_cell(const cell *cell) {
  if (cell) {

    switch (cell->type) {
    case TYPE_CONS:
      printf("CONS");

      break;
    case TYPE_NUM:
      printf("NUM\t%i",cell->value);
      break;
    case TYPE_STR:
      printf("STR\t%s",cell->str);
      break;
    case TYPE_SYM:
      printf("SYM\t%s",cell->sym);
      break;
    case TYPE_FREE:
      printf("FREE\t%p",cell->next_free_cell);
      break;
    }
  } else
    printf("NO CELL");
}