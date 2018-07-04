#include "pifile.h"

void write_program_to_file(char *file_name, char *program_text) {
  // write the program in a file
  FILE *program_file_write = fopen(file_name, "w");
  int results = fputs(program_text, program_file_write);
  if (results == EOF) {
    puts("error writing program file");
    return;
  }
  fclose(program_file_write);
}


cell *parse_file(char *file_path) {
  // execute the file
  FILE *program_file = fopen(file_path, "r");
  if (!program_file) {
    char *err = "file not found: ";
    char *result = malloc(strlen(err) + strlen(file_path) + 1);
    strcpy(result, err);
    strcat(result, file_path);
    pi_error(LISP_ERROR, result);
  }
  cell *res = NULL;
  while (!feof(program_file)) {
    cell *sexpr = read_sexpr(program_file);
    if (sexpr != symbol_file_ended) {
      res = eval(sexpr, memory->global_env);
      cell_remove(res, RECURSIVE);
    }
  }
  fclose(program_file);
  return res;
}