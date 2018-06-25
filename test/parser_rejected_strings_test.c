#include "pilisp.h"
#include "pitestutils.h"
#include <stdio.h>

int parser_test(char *file_path_name) {
  FILE *fp;
  if ((fp = fopen(file_path_name, "r")) == NULL) {
    printf("Error opening file\n");
    return 1;
  }
  jmp_destination = setjmp(env_buf);
  int error = get_last_error();
  if (had_error()) {
    // good: we had an error
    if (error != LISP_ERROR) {
      // there wasn't an error reading bad sexpressions: test failed
      return 1;
    } else {
      // it is a LISP_ERROR: we recognized an invalid sexpression: test is valid!
      reset_error();
      return 0;
    }
  }
  lexer_file(fp);
  fclose(fp);
  return 1; // an invaid sexpression had not rised an error: test failed
}

int main(int argc, char **argv) {
  char *file_path_name = argv[1];
  init_pi();
  // char file_path_name[] = "/home/phreppo/pilisp/test/expressions/badexpressions/badsexpr1.l";
  printf("File name: %s\n", file_path_name);
  return parser_test(file_path_name);
}