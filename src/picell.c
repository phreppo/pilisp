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
  if(next_free_cell == MAX_CELLS)
    pi_error(MEMORY_ERROR,"Ran out of memory");
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

cell *mk_sym(const char *symbol) {
  cell *c = get_cell();
  c->type = TYPE_SYM;
  c->str = malloc(strlen(symbol) + 1);
  strcpy(c->str, symbol);
  return c;
}

cell *mk_cons(cell * car, cell * cdr){
  cell * c = get_cell();
  c->type = TYPE_CONS;
  c->car = car;
  c->cdr = cdr;
  return c;
}

