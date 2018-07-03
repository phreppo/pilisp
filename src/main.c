#include "pilisp.h"
#include "pitestutils.h" // remove once tests

int main(int argc, char **argv) {
  init_pi();
  // mem_prompt();

  if (argc > 1 &&
      (strcmp(argv[1], "-h") == 0 || strcmp(argv[1], "--help") == 0)) {
    printf("\npilisp [<file1.lisp> ... [ <fileN.lisp>]]\n");
    return 0;
  }

  if (argc > 1) {
    jmp_destination = setjmp(env_buf);
    if (had_error()) {
      exit(1);
    }
    // parse one or more files
    unsigned long i = 1;
    for (i = 1; i < argc; i++) {
      cell *res = parse_file(argv[i]);
      print_sexpr(res);
      puts("");
    }
  } else {
    pi_prompt();
  }

  // FILE * f = fopen("/home/phreppo/pilisp/test/expressions/atom.l","r");
  // lexer_file(f);
  // lexer_prompt();
  // parse_prompt();
  // eval_prompt();

  free_pi();
  return 0;
}