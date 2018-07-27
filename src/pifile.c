#include "pifile.h"

void write_program_to_file(char *file_name, char *program_text) {
  // write the program in a file
  FILE *program_file_write = fopen(file_name, "w");
  int results = fputs(program_text, program_file_write);
  if (results == EOF)
    pi_error(MEMORY_ERROR, "error writing program file");
  fclose(program_file_write);
}

cell *parse_file(char *file_path) {
  // execute the file
  FILE *program_file = fopen(file_path, "r");
  if (!program_file) {
    char *err = "file not found: ";
    char result[ERROR_MESSAGE_LEN];
    strcpy(result, err);
    strcat(result, file_path);
    pi_error(LISP_ERROR, result);
  }
  cell *res = NULL;
  while (!feof(program_file)) {
    cell *sexpr = read_sexpr(program_file);
    if (sexpr != symbol_file_ended) {
      res = eval(sexpr, memory->global_env);
      cell_remove_recursive(res);
    }
  }
  fclose(program_file);
  return res;
}

void write_compiler_expression_to_file(char *file_name, cell *to_compilate) {
  FILE *program_file_write = fopen(file_name, "w");

  int results = fputs("(plc '", program_file_write);
  if (results == EOF)
    pi_error(MEMORY_ERROR, "error writing program file");

  print_sexpr_to_file(to_compilate, program_file_write);

  results = fputs(")", program_file_write);
  if (results == EOF)
    pi_error(MEMORY_ERROR, "error writing program file");

  fclose(program_file_write);
}

void write_compiler_to_file(char *file_name) {
  FILE *program_file_write = fopen(file_name, "w");

  int results = fputs(get_compiler_source_hardcoded(), program_file_write);
  if (results == EOF)
    pi_error(MEMORY_ERROR, "error writing program file");
  fclose(program_file_write);
}