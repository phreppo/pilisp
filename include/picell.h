/** @defgroup picell
 *
 *  @brief Provides the data structures for LISP, like cells
 *
 */

/** @addtogroup picell */
/*@{*/
#ifndef PICELL_H
#define PICELL_H
#include "pisettings.h"
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
  TYPE_KEYWORD,
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
      union {
        struct { // struct for builtin lambda: pointer to builtin function and pointer to builtin stack version
          struct cell *(*bl)(
              struct cell *args); // pointer to builtin lambda function
          void *(*bs)(size_t stack_base, unsigned char nargs);
        };
        struct cell *(*bm)(
            struct cell *args,
            struct cell *env); // pointer to builtin macro function
      };
    };
    int value;
    char *str;
    struct cell *next_free_cell;
  };
} cell;

// unsafe unmark: no checks if cell is empty or a builtin! use only if you are
// sure that cell exists and is not a builtin symbol. it's faster
#if INLINE_FUNCTIONS
inline void unsafe_cell_remove(cell *c) { c->marks--; }
#else
void unsafe_cell_remove(cell *c);
#endif

/********************************************************************************
 *                                     STACK DEFINITION
 ********************************************************************************/

size_t stack_pointer;
cell *stack[STACK_LIMIT];

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

// cells space: array of cell blocks. Just one of this will be instantiated:
// the pointer "memory" that represents the allocated cells in the interpreter
typedef struct {
  size_t cell_space_size;
  size_t cell_space_capacity;
  size_t n_cells;
  size_t n_free_cells;
  cell *first_free;
  cell *global_env;
  cell_block *blocks;
} cell_space;

cell_space *memory;
void init_memory();
void free_memory();

cell_space *cell_space_create();
cell *cell_space_get_cell(cell_space *cs);
cell *cell_space_is_symbol_allocated(cell_space *cs, const char *symbol);
void cell_space_init(cell_space *cs);
void cell_space_grow(cell_space *cs);
void cell_space_double_capacity_if_full(cell_space *cs);
void cell_space_mark_cell_as_free(cell_space *cs, cell *c);
void cell_space_free(cell_space *cs);
bool cell_space_is_full(const cell_space *cs);

/********************************************************************************
 *                                CELL BASIC OPERATIONS
 ********************************************************************************/

#if INLINE_FUNCTIONS
inline cell *get_cell() { return cell_space_get_cell(memory); }

inline cell *mk_num(const int n) {
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

inline cell *mk_str(const char *s) {
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

inline cell *mk_cons(cell *car, cell *cdr) {
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
#else
cell *get_cell();
cell *mk_num(const int n);
cell *mk_str(const char *s);
cell *mk_cons(cell *car, cell *cdr);
#endif

cell *mk_sym(const char *symbol);
cell *mk_builtin_lambda(const char *symbol, cell *(*function)(cell *), void *(*builtin_stack)(size_t,unsigned char));
cell *mk_builtin_macro(const char *symbol, cell *(*function)(cell *, cell *));

cell *copy_cell(const cell *c);
void free_cell_pointed_memory(cell *c);

/********************************************************************************
 *                                CELL IDENTIFICATION
 ********************************************************************************/
#if INLINE_FUNCTIONS
inline bool is_num(const cell *c) { return c->type == TYPE_NUM; }
inline bool is_str(const cell *c) { return c->type == TYPE_STR; }
inline bool is_cons(const cell *c) { return c->type == TYPE_CONS; }
inline bool is_keyword(const cell *c) { return c->type == TYPE_KEYWORD; }
inline bool is_sym(const cell *c) {
  return c->type == TYPE_SYM || c->type == TYPE_BUILTINLAMBDA ||
         c->type == TYPE_BUILTINMACRO || c->type == TYPE_KEYWORD;
}
inline bool is_builtin(const cell *c) {
  return c->type == TYPE_BUILTINLAMBDA || c->type == TYPE_BUILTINMACRO;
}
inline bool is_builtin_lambda(const cell *c) {
  return c->type == TYPE_BUILTINLAMBDA;
}
inline bool is_builtin_macro(const cell *c) {
  return c->type == TYPE_BUILTINMACRO;
}
#else
bool is_num(const cell *c);
bool is_str(const cell *c);
bool is_cons(const cell *c);
bool is_sym(const cell *c);
bool is_keyword(const cell *c);
bool is_builtin(const cell *c);
bool is_builtin_lambda(const cell *c);
bool is_builtin_macro(const cell *c);
#endif
cell *is_symbol_builtin_lambda(const char *symbol);
cell *is_symbol_builtin_macro(const char *symbol);
bool cell_is_in_global_env(const cell *global_env, const cell *c);

/********************************************************************************
 *                                  CELL PROTECTION
 ********************************************************************************/

#if INLINE_FUNCTIONS
inline void cell_push(cell *val) {
#if COLLECT_GARBAGE
  val->marks++;
#endif
}

inline void cell_remove(cell *val) {
#if COLLECT_GARBAGE
  if (!val || is_builtin(val))
    return;
  if (val->marks > 0)
    val->marks--;
#if ERROR_EMPTY_REMOVING
  else
    pi_error(MEMORY_ERROR, "you have no more access to that cell");
#endif
#endif
}
#else
inline void cell_push(cell *val);
inline void cell_remove(cell *val);
#endif
void cell_push_recursive(cell *c); // mark as used
void cell_remove_recursive(cell *c);
void cell_remove_args(const cell *args);
void cell_remove_pairlis(const cell *new_env, const cell *old_env);
void cell_remove_cars(const cell *list);
void cell_remove_pairlis_deep(const cell *new_env, const cell *old_env);
/********************************************************************************
 *                                 CORE OF THE GC
 ********************************************************************************/

void collect_garbage(cell_space *cs);
void deep_collect_garbage(cell_space *cs);
void mark_memory(cell_space *cs);
void mark(cell *root);
void sweep(cell_space *cs);
void deep_sweep(cell_space *cs);

#endif // !PICELL_H
       /*@}*/