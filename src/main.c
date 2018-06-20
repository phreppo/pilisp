#include "pilisp.h"
#include "pitestutils.h" // TODO: remove on production

int main(int argc, char **argv) {
  if (argc > 1 &&
      (strcmp(argv[1], "-h") == 0 || strcmp(argv[1], "--help") == 0)) {
    printf("\npilisp [<file1.l> ... [ <fileN.l>]]\n");
    return 0;
  }

  init_env();

  if (argc > 1) {
    // parse one or more files
  } else {
    pi_prompt();
  }

  // FILE * f = fopen("/home/phreppo/pilisp/test/expressions/atom.l","r");
  // lexer_file(f);
  // lexer_prompt();
  // parse_prompt();
//   eval_prompt();
  return 0;
}