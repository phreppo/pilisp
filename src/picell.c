#include "picell.h"
#include "pierror.h"

/**
 * @brief array containing the cells
 *
 */
static cell cells[MAX_CELLS];

/**
 * @brief index of the next free cell in the array
 *
 */
static unsigned long next_free_cell = 0;

cell *get_cell() {
  if (next_free_cell == MAX_CELLS)
    pi_error(MEMORY_ERROR, "Ran out of memory");
  return &(cells[next_free_cell++]);
}

cell *mk_num(const int n) {
  cell *c = get_cell();
  c->type = TYPE_NUM;
  c->value = n;
  return c;
}

cell *mk_str(const char *s) {
  cell *c = get_cell();
  c->type = TYPE_STR;
  c->str = malloc(strlen(s) + 1);
  strcpy(c->str, s);
  return c;
}

static cell * is_symbol_allocated(const char * symbol){
  int i=0;
  for(i=0;i<next_free_cell;i++){
    if(cells[i].type == TYPE_SYM && strcmp(cells[i].sym,symbol) == 0)
      return &cells[i];
  }
  return NULL;
}

cell *mk_sym(const char *symbol) {
  // was the symbol allocated
  cell *allocated = is_symbol_allocated(symbol);
  if(allocated)
    // the symbol was allocated
    return allocated;
  cell *c = get_cell();
  c->type = TYPE_SYM;
  c->str = malloc(strlen(symbol) + 1);
  int i = 0;
  strcpy(c->str, symbol);
  // case unsensitive
  while ((c->str)[i]) {
    c->str[i] = toupper(c->str[i]);
    i++;
  }
  return c;
}

cell *mk_cons(const cell *car,const cell *cdr) {
  cell *c = get_cell();
  c->type = TYPE_CONS;
  c->car = car;
  c->cdr = cdr;
  return c;
}

cell *copy_cell(const cell *c) {
  if (!c)
    return NULL;
  // there is a cell
  cell *copy = NULL;
  if (atom(c)) {
    switch (c->type) {
    case TYPE_NUM:
      copy = mk_num(c->value);
      break;
    case TYPE_STR:
      copy = mk_str(c->str);
      break;
    case TYPE_SYM:
      copy = mk_sym(c->sym);
      break;
    default:
      break;
    }
  } else
    // we have a cons
    copy = mk_cons(copy_cell(car(c)), copy_cell(cdr(c)));
  return copy;
}

int is_num(const cell *c) { return c->type == TYPE_NUM; }
int is_str(const cell *c) { return c->type == TYPE_STR; }
int is_sym(const cell *c) { return c->type == TYPE_SYM; }
//||c->type==TYPE_KEYWORD||c->type==TYPE_BUILTINLAMBDA||c->type==TYPE_BUILTINMACRO||c->type==TYPE_BUILTINSTACK||c->type==TYPE_CXR;}
int is_cons(const cell *c) { return c->type == TYPE_CONS; }