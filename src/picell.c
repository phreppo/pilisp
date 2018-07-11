#include "picell.h"
#include "pierror.h"

cell *get_cell() { return cell_space_get_cell(memory); }

cell *mk_num(const int n) {
  cell *c = get_cell();
  c->type = TYPE_NUM;
  c->value = n;
#if DEBUG_PUSH_REMOVE_MODE
  printf(ANSI_COLOR_BLUE " > Pushing to the stack a num: " ANSI_COLOR_RESET);
  print_sexpr(c);
  puts("");
#endif
  return c;
}

cell *mk_str(const char *s) {
  cell *c = get_cell();
  c->type = TYPE_STR;
  c->str = malloc(strlen(s) + 1);
  strcpy(c->str, s);
#if DEBUG_PUSH_REMOVE_MODE
  printf(ANSI_COLOR_BLUE " > Pushing to the stack a str: " ANSI_COLOR_RESET);
  print_sexpr(c);
  puts("");
#endif
  return c;
}

static cell *is_symbol_allocated(const char *symbol) {
  return cell_space_is_symbol_allocated(memory, symbol);
}

cell *cell_space_is_symbol_allocated(cell_space *cs, const char *symbol) {
  size_t block_index = 0;
  for (block_index = 0; block_index < cs->cell_space_size; block_index++) {

    size_t cell_index = 0;
    cell_block *current_block = cs->blocks + block_index;

    for (cell_index = 0; cell_index < current_block->block_size; cell_index++) {
      cell *current_cell = current_block->block + cell_index;
      if (is_sym(current_cell) && strcmp(current_cell->sym, symbol) == 0) {
        // found!
        return current_cell;
      }
    }
  }
  return NULL;
}

cell *is_symbol_builtin_lambda(const char *symbol) {
  size_t i = 0;
  for (i = 0; i < builtin_lambdas_index; i++) {

    if (strcmp(BUILTIN_LAMBDAS[i].sym, symbol) == 0)
      // found!
      return BUILTIN_LAMBDAS + i;
  }
  // not found
  return NULL;
}

cell *is_symbol_builtin_macro(const char *symbol) {
  size_t i = 0;
  for (i = 0; i < builtin_macros_index; i++) {

    if (strcmp(BUILTIN_MACROS[i].sym, symbol) == 0)
      // found!
      return BUILTIN_MACROS + i;
  }
  // not found
  return NULL;
}

cell *mk_sym(const char *symbol) {
  // check if is a builtin lambda
  cell *allocated = is_symbol_builtin_lambda(symbol);
  if (allocated)
    return allocated;

  allocated = is_symbol_builtin_macro(symbol);
  if (allocated)
    return allocated;

  // was the symbol allocated but no builtin?
  allocated = is_symbol_allocated(symbol);
  if (allocated) {
    // the symbol was allocated
    cell_push(allocated, SINGLE);
    return allocated;
  }
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
#if DEBUG_PUSH_REMOVE_MODE
  printf(ANSI_COLOR_BLUE " > Pushing to the stack a sym: " ANSI_COLOR_RESET);
  print_sexpr(c);
  puts("");
#endif
  return c;
}

cell *mk_cons(cell *car, cell *cdr) {
  cell *c = get_cell();
  c->type = TYPE_CONS;
  c->car = car;
  c->cdr = cdr;
#if DEBUG_PUSH_REMOVE_MODE
  printf(ANSI_COLOR_LIGHT_BLUE
         " > Pushing to the stack a cons: " ANSI_COLOR_RESET);
  print_sexpr(c);
  puts("");
#endif
  return c;
}

cell *mk_builtin_lambda(const char *symbol, cell *(*function)(cell *)) {
  cell *lambda = &BUILTIN_LAMBDAS[builtin_lambdas_index++];
  lambda->type = TYPE_BUILTINLAMBDA;
  lambda->sym = malloc(strlen(symbol) + 1);
  lambda->bl = function;
  int i = 0;
  strcpy(lambda->str, symbol);
  // case unsensitive
  while ((lambda->str)[i]) {
    lambda->str[i] = toupper(lambda->str[i]);
    i++;
  }
  return lambda;
}

cell *mk_builtin_macro(const char *symbol, cell *(*function)(cell *, cell *)) {
  cell *macro = &BUILTIN_MACROS[builtin_macros_index++];
  macro->type = TYPE_BUILTINMACRO;
  macro->sym = malloc(strlen(symbol) + 1);
  macro->bm = function;
  int i = 0;
  strcpy(macro->str, symbol);
  // case unsensitive
  while ((macro->str)[i]) {
    macro->str[i] = toupper(macro->str[i]);
    i++;
  }
  return macro;
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
    case TYPE_BUILTINLAMBDA:
    case TYPE_SYM:
      copy = mk_sym(c->sym);
      break;
    default:
      break;
    }
  } else if (is_cons(c))
    copy = mk_cons(copy_cell(car(c)), copy_cell(cdr(c)));
  return copy;
}

bool is_num(const cell *c) { return c->type == TYPE_NUM; }
bool is_str(const cell *c) { return c->type == TYPE_STR; }
bool is_sym(const cell *c) {
  return c->type == TYPE_SYM || c->type == TYPE_BUILTINLAMBDA ||
         c->type == TYPE_BUILTINMACRO;
}
bool is_cons(const cell *c) { return c->type == TYPE_CONS; }
bool is_builtin(const cell *c) {
  return is_builtin_lambda(c) || is_builtin_macro(c);
}
bool is_builtin_lambda(const cell *c) { return c->type == TYPE_BUILTINLAMBDA; }
bool is_builtin_macro(const cell *c) { return c->type == TYPE_BUILTINMACRO; }

void free_cell_pointed_memory(cell *c) {
  if (c) {
    if (is_str(c) && c->str)
      free(c->str);
    else if (is_sym(c) && c->sym)
      free(c->sym);
  }
}

/********************************************************************************
 *                                  GARBAGE COLLECTOR
 ********************************************************************************/

cell_block *cell_block_create(size_t s) {
  cell_block *new_cb = (cell_block *)malloc(sizeof(cell_block));
  new_cb->block_size = s;
  new_cb->block = (cell *)malloc(s * sizeof(cell));

  size_t i = 0;
  for (i = 0; i < s - 1; i++) {
    (new_cb->block[i]).type = TYPE_FREE;
    (new_cb->block[i]).marked = 0;

    // NEW
    (new_cb->block[i]).marks = 0;

    // set the next next cell as the next free
    (new_cb->block[i]).next_free_cell = (new_cb->block) + i + 1;
  }
  // last cell
  (new_cb->block[s - 1]).type = TYPE_FREE;
  (new_cb->block[s - 1]).marks = 0;
  (new_cb->block[s - 1]).next_free_cell = NULL;
  return new_cb;
}

void cell_space_init(cell_space *cs) {
  // create the space with one block
  cs->cell_space_size = 1;
  cs->cell_space_capacity = INITIAL_BLOCKS;
  // take the space for blocks
  cs->blocks = (cell_block *)malloc(sizeof(cell_block) * INITIAL_BLOCKS);
  // first block
  cs->blocks[0] = *cell_block_create(INITIAL_BLOCK_SIZE);
  cs->first_free = cs->blocks->block;
  cs->n_cells = cs->blocks[0].block_size;
  cs->n_free_cells = INITIAL_BLOCK_SIZE;
}

cell_space *cell_space_create() {
  cell_space *new_space = malloc(sizeof(cell_space));
  cell_space_init(new_space);
  return new_space;
}

bool cell_space_is_full(const cell_space *cs) {
  return cs->cell_space_size >= cs->cell_space_capacity;
}

void cell_space_double_capacity_if_full(cell_space *cs) {
  if (cs->cell_space_size >= cs->cell_space_capacity) {
    // double vector->capacity and resize the allocated memory accordingly
    cs->cell_space_capacity =
        (cs->cell_space_capacity <= 0 ? 1 : cs->cell_space_capacity * 2);
    cs->blocks = (cell_block *)realloc(cs->blocks, sizeof(cell_block) *
                                                       cs->cell_space_capacity);
  }
}

void cell_space_grow(cell_space *cs) {
  // make sure there's room to expand into
  cell_space_double_capacity_if_full(cs);

  // append the value and increment vector->size
  size_t index = cs->cell_space_size;
  // the new block will have the double size of the last block
  cell_block *new = cell_block_create(cs->blocks[index - 1].block_size * 2);
  cs->blocks[index] = *new;
  free(new);

  // new cell block
  cell_block *new_cb = &(cs->blocks[index]);
  // last cell in the new block
  cell *last = new_cb->block + new_cb->block_size - 1;
  // hooking the first free
  last->next_free_cell = cs->first_free;
  cs->first_free = new_cb->block;

  // update the number of cells
  cs->n_cells += new_cb->block_size;
  cs->n_free_cells += new_cb->block_size;

  // update the size
  cs->cell_space_size++;
}

cell *cell_space_get_cell(cell_space *cs) {
  // no space?
  if (!cs->first_free) {
    // then collect garbage
    collect_garbage(cs);
    if ((double)((double)cs->n_free_cells / (double)cs->n_cells) <
        NEW_BLOCK_THRESHOLD) {
#if DEBUG_GARBAGE_COLLECTOR_MODE
      printf(" > Allocating a new block\n");
#endif
      // free/n->cells is smaller than the threshold to allocate a new block =>
      // allocate!
      cell_space_grow(cs);
    }
  }
  cs->n_free_cells--;
  cell *new_cell = cs->first_free;
  if (new_cell) {
    // there will always be a new cell!
    new_cell->marked = 0;
    cs->first_free = new_cell->next_free_cell;
  }
  cell_push(new_cell, SINGLE);
  return new_cell;
}

void init_memory() { memory = cell_space_create(); }

void collect_garbage(cell_space *cs) {
#if COLLECT_GARBAGE
#if DEBUG_GARBAGE_COLLECTOR_MODE
  printf(ANSI_COLOR_YELLOW
         "=================================== Going to collect garbage "
         "===================================\n" ANSI_COLOR_RESET);
  mem_dump(NULL);
#endif
  mark_memory(memory);
  sweep(memory);
#if DEBUG_GARBAGE_COLLECTOR_MODE
  printf(ANSI_COLOR_YELLOW
         "=================================== After sweep "
         "===================================\n" ANSI_COLOR_RESET);
  mem_dump(NULL);
#endif
#endif
}

void mark_memory(cell_space *cs) {
  size_t block_index = 0;
  cell_block *current_block;
  cell *current_cell;
  size_t cell_index;
  for (block_index = 0; block_index < cs->cell_space_size; block_index++) {

    cell_index = 0;
    current_block = cs->blocks + block_index;

    for (cell_index = 0; cell_index < current_block->block_size; cell_index++) {
      current_cell = current_block->block + cell_index;
      if (current_cell->marks > 0 && !(current_cell->type == TYPE_FREE))
        mark(current_cell);
    }
  }
}

void mark(cell *root) {
  if (root && (!root->marked)) {
    root->marked = 1;
    if (is_cons(root)) {
      mark(car(root));
      mark(cdr(root));
    }
  }
}

void sweep(cell_space *cs) {
  size_t block_index = 0;
  cell_block *current_block;
  cell *current_cell;
  for (block_index = 0; block_index < cs->cell_space_size; block_index++) {

    size_t cell_index = 0;
    current_block = cs->blocks + block_index;

    for (cell_index = 0; cell_index < current_block->block_size; cell_index++) {
      current_cell = current_block->block + cell_index;
      if (!current_cell->marked && !(current_cell->type == TYPE_FREE))
        cell_space_mark_cell_as_free(cs, current_cell);
    }
  }
}

void cell_space_mark_cell_as_free(cell_space *cs, cell *c) {
  free_cell_pointed_memory(c);
  c->type = TYPE_FREE;
  c->next_free_cell = cs->first_free;
  cs->first_free = c;
  cs->n_free_cells++;
}

void cell_push(cell *val, unsigned char mode) {
#if COLLECT_GARBAGE
  if (!val)
    return;
  if (is_builtin(val))
    return;

  // if (val->marks < MARKS_LIMIT)
    val->marks++;

  if (mode == RECURSIVE && val && is_cons(val)) {
    if (car(val))
      cell_push(car(val), mode);
    if (cdr(val))
      cell_push(cdr(val), mode);
  }
#endif
}

void cell_push_recursive(cell *val) {
#if COLLECT_GARBAGE
  if (!val)
    return;
  if (is_builtin(val))
    return;

  // if (val->marks < MARKS_LIMIT)
    val->marks++;

  if (is_cons(val)) {
    if (car(val))
      cell_push_recursive(car(val));
    if (cdr(val))
      cell_push_recursive(cdr(val));
  }
#endif
}

void cell_remove_recursive(cell *val) {
#if COLLECT_GARBAGE

#if DEBUG_PUSH_REMOVE_MODE
  printf(ANSI_COLOR_YELLOW " > Removing from the stack: " ANSI_COLOR_RESET);
  print_sexpr(val);
  puts("");
#endif
  if (!val)
    return;
  if (!is_builtin(val)) {

    // if you have errors place these to null
    cell *car1;
    cell *cdr1;

    // NEW
    if (val->marks > 0)
      val->marks--;
#if ERROR_EMPTY_REMOVING
    else
      pi_error(MEMORY_ERROR, "you have no more access to that cell");
#endif

    if (is_cons(val)) {
      car1 = car(val);
      cdr1 = cdr(val);
      if (car1)
        cell_remove_recursive(car1);
      if (cdr1)
        cell_remove_recursive(cdr1);
    }
#if DEBUG_PUSH_REMOVE_MODE
    printf(ANSI_COLOR_GREEN " > Removed from the stack:  " ANSI_COLOR_RESET);
    print_sexpr(val);
    puts("");
#endif
  }
#if DEBUG_PUSH_REMOVE_MODE
  else {
    printf(ANSI_COLOR_DARK_GRAY
           " > Trying to remove a builtin symbol: " ANSI_COLOR_RESET);
    print_sexpr(val);
    puts("");
  }
#endif
#endif
}

void cell_remove(cell *val) {
#if COLLECT_GARBAGE

#if DEBUG_PUSH_REMOVE_MODE
  printf(ANSI_COLOR_YELLOW " > Removing from the stack: " ANSI_COLOR_RESET);
  print_sexpr(val);
  puts("");
#endif
  if (!val)
    return;
  if (!is_builtin(val)) {
    // NEW
    if (val->marks > 0)
      val->marks--;
#if ERROR_EMPTY_REMOVING
    else
      pi_error(MEMORY_ERROR, "you have no more access to that cell");
#endif

#if DEBUG_PUSH_REMOVE_MODE
    printf(ANSI_COLOR_GREEN " > Removed from the stack:  " ANSI_COLOR_RESET);
    print_sexpr(val);
    puts("");
#endif
  }
#if DEBUG_PUSH_REMOVE_MODE
  else {
    printf(ANSI_COLOR_DARK_GRAY
           " > Trying to remove a builtin symbol: " ANSI_COLOR_RESET);
    print_sexpr(val);
    puts("");
  }
#endif
#endif
}

void cell_remove_cars(const cell *list) {
  const cell *act = list;
  cell *tmp;
  while (act) {
    tmp = cdr(act);
    cell_remove_recursive(car(act));
    act = tmp;
  }
}

void cell_remove_args(const cell *args) {
  const cell *act = args;
  cell *tmp;
  while (act) {
    tmp = cdr(act);
    cell_remove(act);
    act = tmp;
  }
}

void cell_remove_pairlis(const cell *new_env, const cell *old_env) {
  const cell *act = new_env;
  while (act != old_env) {
    // for the head of the pairlis
    cell *tmp = cdr(act);
    cell_remove(car(act));
    cell_remove(act);
    act = tmp;
  }
}

void cell_remove_pairlis_deep(const cell *new_env, const cell *old_env) {
  const cell *act = new_env;
  while (act != old_env) {
    // for the head of the pairlis
    cell *tmp = cdr(act);
    cell_remove_recursive(car(act));
    cell_remove(act);
    act = tmp;
  }
}

void cell_stack_remove_cars(const cell *list) {
  const cell *act = list;
  cell *tmp;
  while (act) {
    tmp = cdr(act);
    cell_remove_recursive(car(act));
    act = tmp;
  }
}

void cell_space_free(cell_space *cs) {
  if (cs) {
    size_t block_index;
    for (block_index = 0; block_index < cs->cell_space_size; block_index++)
      cell_block_free((cs->blocks) + block_index);
    free(cs->blocks);
    free(cs);
  }
}

void cell_block_free(cell_block *cb) {
  if (cb) {
    size_t cell_index = 0;
    for (cell_index = 0; cell_index < cb->block_size; cell_index++)
      // free the memory pointed from strings and symbols
      free_cell_pointed_memory(cb->block + cell_index);

    free(cb->block);
  }
}

void free_memory() { cell_space_free(memory); }

bool cell_is_in_global_env(const cell *global_env, const cell *c) {
  if (!global_env)
    // no global env
    return false;
  if (c == global_env)
    return true;
  if (!is_cons(global_env))
    return false;
  return cell_is_in_global_env(car(global_env), c) ||
         cell_is_in_global_env(cdr(global_env), c);
}