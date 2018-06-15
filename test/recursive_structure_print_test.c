#include "pilisp.h"
#include "pitestutils.h"
#include <stdio.h>

int main(int argc, char **argv) {
  cell * head = mk_cons(mk_num(1),NULL);
  cell * second = mk_cons(head,NULL);
  head->cdr = second;
  print_sexpr_mode(head,SEXPR_PRINT_VERBOSE);
  print_sexpr_mode(head,SEXPR_PRINT_DEFAULT);
  return 0;
}