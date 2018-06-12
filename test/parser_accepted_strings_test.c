#include "pilisp.h"
#include "pitestutils.h"
#include <stdio.h>

int parser_test(char *file_path_name) {
  FILE *fp;
  if ((fp = fopen(file_path_name, "r")) == NULL) {
    printf("Error opening file\n");
    exit(1);
  }
  jmp_destination = setjmp(env_buf);
  if(get_last_error() == LISP_ERROR){
    // there was an error reading good sexpressions
    return 1;
  }
  lexer_file(fp);
  fclose(fp);
  return 0;
}

int main(int argc, char **argv) {
  char *file_path_name = argv[1];
  printf("File name: %s\n", file_path_name);
  return parser_test(file_path_name);
}