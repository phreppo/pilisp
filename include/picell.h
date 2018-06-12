/** @defgroup picell
 *
 * This module provides the data structures for LISP, like cells
 *
 */

/** @addtogroup picell */
/*@{*/

#ifndef PICELL
#define PICELL

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

cell *get_cell();

cell *mk_num(int n);

cell *mk_str(const char *s);

cell *mk_sym(const char *symbol);


#endif // !PICELL

/*@}*/