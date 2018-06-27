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
  printf(ANSI_COLOR_LIGHT_BLUE " > Pushing to the stack a cons: " ANSI_COLOR_RESET);
  print_sexpr(c);
  puts("");
#endif
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

cell_block *new_cell_block(size_t s) {
  cell_block *new_cb = (cell_block *)malloc(sizeof(cell_block));
  new_cb->block_size = s;
  new_cb->block = (cell *)malloc(s * sizeof(cell));
  size_t i = 0;
  for (i = 0; i < s - 1; i++) {
    (new_cb->block[i]).type = TYPE_FREE;
    (new_cb->block[i]).marked = 0;
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
  cs->blocks = (cell_block *)malloc(sizeof(cell_block) * INITIAL_BLOCKS);
  // first block
  cs->blocks[0] = *new_cell_block(INITIAL_BLOCK_SIZE);
  cs->first_free = cs->blocks->block;
  cs->n_cells = cs->blocks[0].block_size;
  cs->n_free_cells = INITIAL_BLOCK_SIZE;
  cs->stack = cell_stack_create();
}

bool cell_space_is_full(const cell_space *cs) {
  return cs->cell_space_size >= cs->cell_space_capacity;
}

void cell_space_double_capacity_if_full(cell_space *cs) {
  if (cs->cell_space_size >= cs->cell_space_capacity) {
    // double vector->capacity and resize the allocated memory accordingly
    cs->cell_space_capacity *= 2;
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
  cs->blocks[index] = *new_cell_block(cs->blocks[index - 1].block_size * 2);

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
    cell_space_grow(cs);
  }
  cs->n_free_cells--;
  cell *new_cell = cs->first_free;
  new_cell->marked = 0;
  cs->first_free = new_cell->next_free_cell;
  cell_push(new_cell);
  return new_cell;
}

void init_memory() {
  memory = malloc(sizeof(cell_space));
  cell_space_init(memory);
}

void collect_garbage(cell_space *cs) {
#if DEBUG_GARBAGE_COLLECTOR_MODE
  printf(ANSI_COLOR_YELLOW
         " >> Going to collect garbage <<\n" ANSI_COLOR_RESET);
  print_cell_space(memory);
#endif
  cell_stack * stack = cs->stack;
  cell_stack_node * node = cs->stack->head;
  while(node){
    mark(node->c);
    node= node->next;
  }
#if DEBUG_GARBAGE_COLLECTOR_MODE
  printf(ANSI_COLOR_YELLOW " >> After marking <<\n" ANSI_COLOR_RESET);
  print_cell_space(memory);
#endif
  sweep(memory);
#if DEBUG_GARBAGE_COLLECTOR_MODE
  printf(ANSI_COLOR_YELLOW " >> After sweep <<\n" ANSI_COLOR_RESET);
  print_cell_space(memory);
#endif
}

void mark(cell *root) {
  if (root) {
    root->marked = 1;
    if (is_cons(root)) {
      mark(car(root));
      mark(cdr(root));
    }
  }
}

void sweep(cell_space *cs) {
  size_t block_index = 0;
  for (block_index = 0; block_index < cs->cell_space_size; block_index++) {

    size_t cell_index = 0;
    cell_block *current_block = cs->blocks + block_index;

    for (cell_index = 0; cell_index < current_block->block_size; cell_index++) {
      cell *current_cell = current_block->block + cell_index;
      if (!current_cell->marked && !(current_cell->type == TYPE_FREE)) {
        cell_space_mark_cell_as_free(cs, current_cell);
      } else {
        current_cell->marked = 0;
      }
    }
  }
}

void cell_space_mark_cell_as_free(cell_space *cs, cell *c) {
  free_cell_pointed_memory(c);
  c->type = TYPE_FREE;
  c->next_free_cell = cs->first_free;
  cs->first_free = c;
  cs->n_free_cells++;
  // ! HERE PUT THE POP FROM THE STACK
}

cell_stack *cell_stack_create() {
  cell_stack *s = malloc(sizeof(cell_stack));
  s->head = NULL;
  s->tail = NULL;
  return s;
}

cell_stack_node *cell_stack_node_create_node(cell *val, cell_stack_node *next,
                                             cell_stack_node *prec) {

  cell_stack_node *n = malloc(sizeof(cell_stack_node));
  n->c = val;
  n->next = next;
  n->prec = prec;
  return n;
}

void cell_stack_push(cell_stack *stack, cell *val) {
  cell_stack_node *n = cell_stack_node_create_node(val, NULL, NULL);
  if (stack->head == NULL) {
    stack->head = n;
    stack->tail = n;
  } else {
    stack->head->prec = n;
    n->next = stack->head;
    stack->head = n;
  }
}
void cell_stack_remove(cell_stack *stack, cell *val) {
#if DEBUG_PUSH_REMOVE_MODE
  printf(ANSI_COLOR_YELLOW " > Removing from the stack: " ANSI_COLOR_RESET);
  print_sexpr(val);
  puts("");
#endif
  // TODO CHECK THIS
  cell_stack_node *act = stack->head;
  cell_stack_node *prec = NULL;
  while (act) {
    if (act->c == val) {
      // found
      
      //is it's a cons we have to remove also their sons
      // if(is_cons(val)){
        // cell_stack_remove(stack, car(val));
        // cell_stack_remove(stack, cdr(val));
      // }
      if (prec) {
        // was not the first in the list
        prec->next = act->next;
        if (act->next)
          // we're not removing the tail
          act->next->prec = prec;
        else
          // we're removing the tail
          stack->tail = prec;
      } else {
        // was the first in the list
        stack->head = act->next;
        if (!stack->head)
          // was the only in the tail
          stack->tail = NULL;
      }
      free(act);
      return;
    }
    prec = act;
    act = act->next;
  }
#if DEBUG_PUSH_REMOVE_MODE
  printf(ANSI_COLOR_RED " > Can't find in the stack: " ANSI_COLOR_RESET);
  print_sexpr(val);
  puts("");
#endif
}

void cell_push(cell *c) { cell_stack_push(memory->stack, c); }

void cell_remove(cell *c) { cell_stack_remove(memory->stack, c); }