/** @defgroup picell
 *
 *  @brief Provides the data structures for LISP, like cells
 *
 */

/** @addtogroup picell */
/*@{*/

#ifndef PICELL_H
#define PICELL_H
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

/**
 * @brief enumeration to identify the type of one cell
 *
 */
enum {
  TYPE_CONS = 0, ///< cons cell: it has car and cdr
  TYPE_SYM,      ///< symbol cell
  TYPE_NUM,      ///< number cell
  TYPE_STR,      ///< string cell
  //   TYPE_KEYWORD,
  //   TYPE_BUILTINLAMBDA,
  //   TYPE_BUILTINMACRO,
  //   TYPE_BUILTINSTACK,
  //   TYPE_CXR,
  //   TYPE_FREE
};

/**
 * @brief type to describe a generic cell
 *
 */
typedef struct cell {
  unsigned char type;
  union {
    struct {
      struct cell *car;
      struct cell *cdr;
    };
    char *sym;
    int value;
    char *str;
  };
  // union {
  //   double dvalue;
  //   long long int lvalue;
  // } numV;
} cell;

/**
 * @brief max number of aviable cells
 *
 */
// #define MAX_CELLS 65536
#define MAX_CELLS 9999999

/**
 * @brief function to get a cell
 *
 * @return cell* pointer to the new cell
 */
cell *get_cell();

/**
 * @brief make a number cell
 *
 * @param n the number
 * @return cell* pointer to the new cell
 */
cell *mk_num(int n);

/**
 * @brief make a string cell
 *
 * @param s the string
 * @return cell* pointer to the new cell
 */
cell *mk_str(const char *s);

/**
 * @brief make a new symbol cell
 *
 * @param symbol name of the symbol
 * @return cell* pointer to the new cell
 */
cell *mk_sym(const char *symbol);

/**
 * @brief make a new cons cell
 *
 * @param car pointer to the car
 * @param cdr pointer to the cdr
 * @return cell* pointer to the new cell
 */
cell *mk_cons(cell *car,cell *cdr);

cell * copy_cell(const cell * c);

int is_num(const cell* c);
int is_str(const cell* c);
int is_sym(const cell* c);
//||c->type==TYPE_KEYWORD||c->type==TYPE_BUILTINLAMBDA||c->type==TYPE_BUILTINMACRO||c->type==TYPE_BUILTINSTACK||c->type==TYPE_CXR;}
int is_cons(const cell* c);

#endif // !PICELL_H

/*@}*/