#include "pilisp.h"
#include "pitestutils.h"
#include <stdio.h>

int print_parser_test(char *file_path_name) {
  FILE *fp;
  if ((fp = fopen(file_path_name, "r")) == NULL) {
    printf("Error opening file\n");
    exit(1);
  }
  jmp_destination = setjmp(env_buf);
  if (get_last_error() == LISP_ERROR) {
    // there was an error reading good sexpressions
    return 1;
  }
  while (!feof(fp)) {
    cell * root = read_sexpr(fp);
    if (root) {
      print_sexpr(root);
      print_sexpr_mode(root, SEXPR_PRINT_DEFAULT);
      print_sexpr_mode(root, SEXPR_PRINT_VERBOSE);
    }
  }
  fseek(fp, 0, SEEK_SET);
  fclose(fp);
  return 0;
}

int main(int argc, char **argv) {
  char * file_path_name = argv[1];
  // char file_path_name[] = "/home/phreppo/pilisp/test/expressions/dotexpressions.lisp";;
  pi_message("Welcome to pilisp");
  printf("File name: %s\n", file_path_name);
  return print_parser_test(file_path_name);
}