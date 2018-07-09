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
};


typedef struct cell {
  unsigned char type, marked;
  union {
    struct {
      struct cell *car;
      struct cell *cdr;
    };
    struct {
      char *sym;
      union {
        struct cell *(*bl)(struct cell *args);
        struct cell *(*bm)(struct cell *args, struct cell *env);
        struct symbol_value_node * value_list;
      };
    };

    int value;
    char *str;
    struct cell *next_free_cell;
  };
} cell;

/********************************************************************************
*                               Symbol value node
********************************************************************************/

typedef struct symbol_value_node{
  cell * assoc;
  struct symbol_value_node * next;
}symbol_value_node;

inline symbol_value_node * create_symbol_value_node(cell * value){
  symbol_value_node * ret = malloc(sizeof(symbol_value_node));
  ret->assoc = value;
  ret->next = NULL;
  return ret;
}


/********************************************************************************
 *                                CELL IDENTIFICATION
 ********************************************************************************/

inline bool is_num(const cell *c) { return c->type == TYPE_NUM; }
inline bool is_str(const cell *c) { return c->type == TYPE_STR; }
inline bool is_sym(const cell *c) {
  return c->type == TYPE_SYM || c->type == TYPE_BUILTINLAMBDA ||
         c->type == TYPE_BUILTINMACRO;
}
inline bool is_cons(const cell *c) { return c->type == TYPE_CONS; }
inline bool is_builtin_lambda(const cell *c) { return c->type == TYPE_BUILTINLAMBDA; }
inline bool is_builtin_macro(const cell *c) { return c->type == TYPE_BUILTINMACRO; }
inline bool is_builtin(const cell *c) {
  return is_builtin_lambda(c) || is_builtin_macro(c);
}

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

inline cell_stack *cell_stack_create() {
  cell_stack *s = malloc(sizeof(cell_stack));
  s->head = NULL;
  s->tail = NULL;
  return s;
}

inline cell_stack_node *cell_stack_node_create_node(cell *val) {
  cell_stack_node *n = malloc(sizeof(cell_stack_node));
  n->c = val;
  n->next = NULL;
  n->prec = NULL;
  return n;
}

void cell_stack_push(cell_stack *stack, cell *val, unsigned char mode);
void cell_stack_remove(cell_stack *stack, const cell *val, unsigned char mode);
void cell_stack_remove_args(cell_stack *stack, const cell *args);
void cell_stack_remove_pairlis(cell_stack *stack, const cell *new_env,
                               const cell *old_env);
void cell_stack_remove_pairlis_deep(cell_stack *stack, const cell *new_env,
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

/********************************************************************************
 *                                CELL BASIC OPERATIONS
 ********************************************************************************/

void init_memory();
void free_memory();

inline cell *get_cell() { return cell_space_get_cell(memory); }

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

cell *mk_sym(const char *symbol);
cell *mk_builtin_lambda(const char *symbol, cell *(*function)(cell *));
cell *mk_builtin_macro(const char *symbol, cell *(*function)(cell *, cell *));
cell *copy_cell(const cell *c);
void free_cell_pointed_memory(cell *c);

// ==================== BASIC ====================

inline int atom(const cell *c) {
  return (c == NULL) // NIL case
         ||
         (c->type == TYPE_SYM || c->type == TYPE_NUM || c->type == TYPE_STR ||
          c->type == TYPE_BUILTINLAMBDA || c->type == TYPE_BUILTINMACRO);
}

inline cell *car(const cell *c) {
  if (c == NULL)
    // (car NIL)
    return NULL;
#if CHECKS
  if (atom(c))
    pi_lisp_error("car applied to an atom");
#endif
  return c->car;
}
inline cell *cdr(const cell *c) {
  if (c == NULL)
    // (cdr NIL)
    return NULL;
#if CHECKS
  if (atom(c))
    pi_lisp_error("cdr applied to an atom");
#endif
  return c->cdr;
}
inline cell *caar(const cell *c) { return car(car(c)); }
inline cell *cddr(const cell *c) { return cdr(cdr(c)); }
inline cell *cadr(const cell *c) { return car(cdr(c)); }
inline cell *cdar(const cell *c) { return cdr(car(c)); }
inline cell *cadar(const cell *c) { return car(cdr(car(c))); }
inline cell *caddr(const cell *c) { return car(cdr(cdr(c))); }
inline cell *cons(cell *car, cell *cdr) { return mk_cons(car, cdr); }
inline bool eq(const cell *v1, const cell *v2) {
  if (!v1 || !v2)
    return (v1 == v2);
  if (is_num(v1) && is_num(v2))
    return (v1->value == v2->value);
  if (is_str(v1) && is_str(v2))
    return (strcmp(v1->str, v2->str) == 0);
  return (v1 == v2);
}

bool total_eq(const cell *c1,
              const cell *c2); // works also on lists: eq does not


/********************************************************************************
 *                                  CELL PROTECTION
 ********************************************************************************/

enum push_remove_mode {
  SINGLE,
  RECURSIVE,
};

inline void cell_push(cell *c, unsigned char mode) {
#if COLLECT_GARBAGE
  cell_stack_push(memory->stack, c, mode);
#endif
}

inline void cell_remove(const cell *c, unsigned char mode) {
#if COLLECT_GARBAGE
  cell_stack_remove(memory->stack, c, mode);
#endif
}

inline void cell_remove_args(const cell *args) {
#if COLLECT_GARBAGE
  cell_stack_remove_args(memory->stack, args);
#endif
}

inline void cell_remove_pairlis(const cell *new_env, const cell *old_env) {
#if COLLECT_GARBAGE
  cell_stack_remove_pairlis(memory->stack, new_env, old_env);
#endif
}

inline void cell_remove_cars(const cell *list) {
#if COLLECT_GARBAGE
  cell_stack_remove_cars(memory->stack, list);
#endif
}

inline void cell_remove_pairlis_deep(const cell *new_env, const cell *old_env) {
#if COLLECT_GARBAGE
  cell_stack_remove_pairlis_deep(memory->stack, new_env, old_env);
#endif
}


#endif // !PICELL_H
       /*@}*/
