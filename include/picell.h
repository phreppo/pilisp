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
  unsigned long marks;
  union {
    struct {
      struct cell *car;
      struct cell *cdr;
    };
    struct {
      char *sym;
      union{
        struct cell* (*bl)(struct cell* args); // pointer to builtin lambda function
        struct cell* (*bm)(struct cell* args,struct cell* env); // pointer to builtin macro function
      };
    };
    int value;
    char *str;
    struct cell *next_free_cell;
  };
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
cell *mk_builtin_lambda(const char *symbol, cell* (*function)(cell*));
cell *mk_builtin_macro(const char *symbol, cell* (*function)(cell*,cell*));

cell *copy_cell(const cell *c);
void free_cell_pointed_memory(cell *c);

/********************************************************************************
 *                                  CELL PROTECTION
 ********************************************************************************/

void cell_push(cell *c);         // mark as used
void cell_push_recursive(cell *c);         // mark as used
void cell_remove(cell *c);       // mark as not used
void cell_remove_recursive(cell *c);                 // faster: no check about the mode
void cell_remove_args(
    const cell *args); // removes from the stack the structure of the args
void cell_remove_pairlis(const cell *new_env, const cell *old_env);
void cell_remove_pairlis_deep(const cell *new_env, const cell *old_env);
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

/********************************************************************************
 *                                 CORE OF THE GC
 ********************************************************************************/

cell_space *memory;

void collect_garbage(cell_space *cs);
void mark_memory(cell_space *cs);
void mark(cell *root);
void sweep(cell_space *cs);

#endif // !PICELL_H
       /*@}*/