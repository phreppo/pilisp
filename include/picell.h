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

enum {
  TYPE_CONS = 0,
  TYPE_SYM,
  //   TYPE_KEYWORD,
  TYPE_NUM,
  TYPE_STR,
  //   TYPE_BUILTINLAMBDA,
  //   TYPE_BUILTINMACRO,
  //   TYPE_BUILTINSTACK,
  //   TYPE_CXR,
  //   TYPE_FREE
};

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

#define MAX_CELLS 65536

static cell cells[MAX_CELLS];

static unsigned long next_free_cell = 0;

cell *get_cell();

cell *mk_num(int n);

cell *mk_str(const char *s);

cell *mk_sym(const char *symbol);

cell *mk_cons(cell * car, cell * cdr);


#endif // !PICELL_H

/*@}*/