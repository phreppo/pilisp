#include "pilisp.h"
#include "pitestutils.h"
#include <stdio.h>

int print_bad_cell_test() {
  jmp_destination = setjmp(env_buf);
  if (had_error()) {
    if (get_last_error() != MODE_ERROR) {
      // there was an error that was'nt the one we were looking for
      return 1;
    } else {
      // good: we had the rigth error
      return 0;
    }
  }
  cell * c = mk_cons(NULL,NULL);
  c->type = -1;
  print_sexpr(c);
  // if you arrive here you printed a bad cell
  return 1;
}

int print_verbose_bad_cell_test() {
  jmp_destination = setjmp(env_buf);
  if (had_error()) {
    if (get_last_error() != MODE_ERROR) {
      // there was an error that was'nt the one we were looking for
      return 1;
    } else {
      // good: we had the rigth error
      return 0;
    }
  }
  cell * c = mk_cons(NULL,NULL);
  c->type = -1;
  print_sexpr_mode(c,SEXPR_PRINT_VERBOSE);
  // if you arrive here you printed a bad cell
  return 1;
}

int print_bad_token_test() {
  jmp_destination = setjmp(env_buf);
  if (had_error()) {
    if (get_last_error() != MODE_ERROR) {
      // there was an error that was'nt the one we were looking for
      return 1;
    } else {
      // good: we had the rigth error
      return 0;
    }
  }
  int tok = -1;
  print_token(tok);
  // if you arrive here you printed a bad cell
  return 1;
}

int main(int argc, char **argv) {

  return print_bad_cell_test() || print_verbose_bad_cell_test() || print_bad_token_test();
}