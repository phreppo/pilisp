#include "pilisp.h"
#include "pitestutils.h"
#include <stdio.h>

int print_lexer_test(char *file_path_name) {
  FILE *fp;
  if ((fp = fopen(file_path_name, "r")) == NULL) {
    printf("Error opening file\n");
    exit(1);
  }
  while (!feof(fp)) {
    int tok = next_token(fp);
    print_token(tok);
  }
  fseek(fp, 0, SEEK_SET);
  fclose(fp);
  return 0;
}

int main(int argc, char **argv) {
  char * file_path_name = argv[1];
  // char file_path_name[] = "/home/phreppo/pilisp/test/expressions/dotexpressions.lisp";;
  printf("File name: %s\n", file_path_name);
  return print_lexer_test(file_path_name);
}