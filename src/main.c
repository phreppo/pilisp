#include "pilisp.h"
#include "pitestutils.h" // TODO: remove on production

int main(int argc, char **argv) {

  cell_block * block= new_cell_block(4);
  // (block->block[1]).type = TYPE_NUM;
  // (block->block[1]).value = 1;
  print_cell_block(block);
  return 0;
  if (argc > 1 &&
      (strcmp(argv[1], "-h") == 0 || strcmp(argv[1], "--help") == 0)) {
    printf("\npilisp [<file1.lisp> ... [ <fileN.lisp>]]\n");
    return 0;
  }

  init_pi();

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
  //   eval_prompt();
  return 0;
}