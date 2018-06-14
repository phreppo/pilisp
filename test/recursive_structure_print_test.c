#include "pilisp.h"
#include "pitestutils.h"
#include <stdio.h>

int main(int argc, char **argv) {
  cell * head = mk_cons(mk_num(1),NULL);
  cell * second = mk_cons(head,NULL);
  head->cdr = second;
  print_sexpr(head,SEXPR_PRINT_DOT);
  return 0;
}