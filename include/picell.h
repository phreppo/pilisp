/** @defgroup picell
 *
 *  @brief Provides the data structures for LISP, like cells
 *
 */

/** @addtogroup picell */
/*@{*/

#ifndef PICELL_H
#define PICELL_H
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

/********************************************************************************
 *                                  CELL DEFINITION
 ********************************************************************************/

enum {
  TYPE_CONS = 0,
  TYPE_SYM,
  TYPE_NUM,
  TYPE_STR,
  TYPE_FREE,
  TYPE_BUILTINLAMBDA,
  TYPE_BUILTINMACRO,
  //   TYPE_KEYWORD,
};

typedef struct cell {
  unsigned char type, marked;
  union {
    struct {
      struct cell *car;
      struct cell *cdr;
    };
    char *sym;
    int value;
    char *str;
    struct cell *next_free_cell;
  };

  // TODO: add pointer for funciton for builtin lambda and builtin macro
} cell;

/********************************************************************************
 *                                CELL BASIC OPERATIONS
 ********************************************************************************/

void init_memory();
void free_memory();

cell *get_cell();
cell *mk_num(int n);
cell *mk_str(const char *s);
cell *mk_sym(const char *symbol);
cell *mk_cons(cell *car, cell *cdr);
cell *mk_builtin_lambda(const char *symbol);
cell *mk_builtin_macro(const char *symbol);

cell *copy_cell(const cell *c);
void free_cell_pointed_memory(cell *c);

/********************************************************************************
 *                                  CELL PROTECTION
 ********************************************************************************/

enum push_remove_mode {
  SINGLE,
  RECURSIVE,
};

void cell_push(cell *c, unsigned char mode);         // mark as used
void cell_remove(const cell *c, unsigned char mode); // mark as not used
void cell_remove_args(
    const cell *args); // removes from the stack the structure of the args
void cell_remove_pairlis(const cell *new_env, const cell *old_env);
void cell_remove_cars(const cell *list);

/********************************************************************************
 *                                CELL IDENTIFICATION
 ********************************************************************************/

bool is_num(const cell *c);
bool is_str(const cell *c);
bool is_sym(const cell *c);
bool is_cons(const cell *c);
bool is_builtin(const cell *c);
bool is_builtin_lambda(const cell *c);
bool is_builtin_macro(const cell *c);
cell *is_symbol_builtin_lambda(const char *symbol);
cell *is_symbol_builtin_macro(const char *symbol);
bool cell_is_in_global_env(const cell *global_env, const cell *c);

/********************************************************************************
 *                              STACK GARBAGE COLLECTOR
 ********************************************************************************/

typedef struct cell_stack_node {
  struct cell_stack_node *prec;
  struct cell_stack_node *next;
  cell *c;
} cell_stack_node;

typedef struct {
  cell_stack_node *head;
  cell_stack_node *tail;
} cell_stack;

cell_stack *cell_stack_create();
cell_stack_node *cell_stack_node_create_node(cell *val);

void cell_stack_push(cell_stack *stack, cell *val, unsigned char mode);
void cell_stack_remove(cell_stack *stack, const cell *val, unsigned char mode);
void cell_stack_remove_args(cell_stack *stack, const cell *args);
void cell_stack_remove_pairlis(cell_stack *stack, const cell *new_env,
                               const cell *old_env);
void cell_stack_remove_cars(
    cell_stack *stack,
    const cell *list); // recursively eliminates the cars in the list
void cell_stack_free(cell_stack *stack);

/********************************************************************************
 *                                  GARBAGE COLLECTOR
 ********************************************************************************/

// cells array
typedef struct {
  size_t block_size;
  cell *block;
} cell_block;

cell_block *cell_block_create(size_t s);
void cell_block_free(cell_block *cb);

// cells space: array of cell blocks. Just one of this will be instantiated: the
// pointer "memory" that represents the allocated cells in the interpreter
typedef struct {
  size_t cell_space_size;
  size_t cell_space_capacity;
  size_t n_cells;
  size_t n_free_cells;
  cell *first_free;
  cell *global_env;
  cell_block *blocks;
  cell_stack *stack;
} cell_space;

// allocates a new block and links the last free cell with the first free in the
// cell space
void cell_space_grow(cell_space *cs);
// doubles the capacity of the cell spce
void cell_space_double_capacity_if_full(cell_space *cs);
// always use this on one allocated cell space before use
void cell_space_init(cell_space *cs);
// create a new cell space
cell_space *cell_space_create();
// true => no free cells
bool cell_space_is_full(const cell_space *cs);
// ALWAYS returns a new cell: if none is present it allocates new space,
// eventually runs gc
cell *cell_space_get_cell(cell_space *cs);
// checks if the symbol is present in the cell space
cell *cell_space_is_symbol_allocated(cell_space *cs, const char *symbol);
// marks the cell as free and updates the first cel, in the cs. cell must be in
// the cell space
void cell_space_mark_cell_as_free(cell_space *cs, cell *c);
// releases the full content of the cs
void cell_space_free(cell_space *cs);
// releases the entire memory in the stack: use only on error
void cell_space_destroy_stack(cell_space *cs);

/********************************************************************************
 *                                 CORE OF THE GC
 ********************************************************************************/

cell_space *memory;

void collect_garbage(cell_space *cs);
void mark(cell *root);
void sweep(cell_space *cs);

#endif // !PICELL_H

/*@}*/