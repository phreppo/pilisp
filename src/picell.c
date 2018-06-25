#include "picell.h"
#include "pierror.h"

// GARBAGE COLLECTOR
static cell *next_free = NULL;

cell_block *new_cell_block(size_t s) {
  cell_block *new_cb = (cell_block *)malloc(sizeof(cell_block));
  new_cb->block_size = s;
  new_cb->block = (cell *)malloc(s * sizeof(cell));
  size_t i = 0;
  for (i = 0; i < s - 1; i++) {
    (new_cb->block[i]).type = TYPE_FREE;
    // set the next next cell as the next free
    (new_cb->block[i]).next_free_cell = (new_cb->block) + i + 1;
  }
  // last cell
  (new_cb->block[s - 1]).type = TYPE_FREE;
  (new_cb->block[s - 1]).next_free_cell = NULL;
  return new_cb;
}

void cell_space_init(cell_space *cs) {
  // create the space with one block
  cs->cell_space_size = 1;
  cs->cell_space_capacity = INITIAL_BLOCKS;
  // take the space for blocks
  cs->blocks = malloc(sizeof(cell_block *) * INITIAL_BLOCKS);
  // first block
  cs->blocks[0] = *new_cell_block(INITIAL_BLOCK_SIZE);
}

bool cell_space_is_full(const cell_space *cs) {
  return cs->cell_space_size >= cs->cell_space_capacity;
}

void cell_space_double_capacity_if_full(cell_space *cs) {
  if (cs->cell_space_size >= cs->cell_space_capacity) {
    // double vector->capacity and resize the allocated memory accordingly
    cs->cell_space_capacity *= 2;
    cs->blocks =
        realloc(cs->blocks, sizeof(cell_block) * cs->cell_space_capacity);
  }
}

void cell_space_grow(cell_space *cs) {
  // make sure there's room to expand into
  cell_space_double_capacity_if_full(cs);

  // append the value and increment vector->size
  size_t index = cs->cell_space_size;
  // the new block will have the double size of the last block
  cs->blocks[index] = *new_cell_block(cs->blocks[index - 1].block_size * 2);
  cs->cell_space_size++;
}

cell_block cell_block_get(cell_space *cs, size_t index) {
  if (index >= cs->cell_space_size || index < 0) {
    printf("Index %d out of bounds for cs of cell_space_size %d\n", index,
           cs->cell_space_size);
    exit(1);
  }
  return cs->blocks[index];
}

void init_memory() {
  memory = malloc(sizeof(cell_space));
  cell_space_init(memory);
}

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

static cell *is_symbol_allocated(const char *symbol) {
  int i = 0;
  for (i = 0; i < next_free_cell; i++) {
    if (cells[i].type == TYPE_SYM && strcmp(cells[i].sym, symbol) == 0)
      return &cells[i];
  }
  return NULL;
}

cell *mk_sym(const char *symbol) {
  // was the symbol allocated
  cell *allocated = is_symbol_allocated(symbol);
  if (allocated)
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

cell *mk_cons(cell *car, cell *cdr) {
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