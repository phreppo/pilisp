#include "pilisp.h"
#include <stdbool.h>
#include <string.h>

bool need_for_help(int argc, char **argv) {
  return argc > 1 &&
         (strcmp(argv[1], "-h") == 0 || strcmp(argv[1], "--help") == 0);
}

void print_help() { printf("\npilisp [<file1.lisp> ... [ <fileN.lisp>]]\n"); }

int main(int argc, char **argv) {

  if (need_for_help(argc, argv)) {
    print_help();
    return 0;
  }

  init_pi();
  if (argc > 1) {
    pi_parse_args_files(argc,argv);
  } else {
    pi_prompt();
  }
  free_pi();
  
  return 0;
}