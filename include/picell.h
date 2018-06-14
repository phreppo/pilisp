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

/**
 * @brief enumeration to identify the type of one cell
 * 
 */
enum {
  TYPE_CONS = 0,            // @brief cons cell: it has car and cdr
  TYPE_SYM,                 // @brief symbol cell
  TYPE_NUM,                 // @brief number cell
  TYPE_STR,                 // @brief string cell
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
      union {
        struct cell *car;
        char *sym;
        int value;
        char *str;
      };
      union {
        struct cell *cdr;
      };
    };
    // union {
    //   double dvalue;
    //   long long int lvalue;
    // } numV;
  };
} cell;

/**
 * @brief max number of aviable cells
 * 
 */
#define MAX_CELLS 65536

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
cell *mk_cons(cell * car, cell * cdr);


#endif // !PICELL_H

/*@}*/