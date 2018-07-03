#include "pilisp.h"
#include <stdio.h>
#include <stdlib.h>

int fpeek(FILE *const fp) {
  const int c = getc(fp);
  return c == EOF ? 0 : ungetc(c, fp);
}

char *string_merge(char *str1, char *str2) {
  char *new_str = malloc(strlen(str1) + strlen(str2) + 1);
  new_str[0] = '\0'; // ensures the memory is an empty string
  strcat(new_str, str1);
  strcat(new_str, str2);
  return new_str;
}

// how to use:
//  [1] = program file path;
//  [2] = program to execute after the first;
//  [3] = sexpression result;
//  [4](optional) = file number. why [4]? beacuse when you run many tests at the
//  same you can occur in file opening errors
//
// Execution: load and parse the [1] program (write there your test functions)
// => execute the program in [2] => check that the result of the last
// sexpression in equal to [3]
int main(int argc, char **argv) {
  init_pi();
  char *load_file_name = argv[1];
  parse_file(load_file_name);

  char *program = argv[2];
  puts(program);

  char *result = argv[3];

  char *file_number = (argc >= 4 ? argv[4] : 0);

  // write the program in a file

  char *program_file_path =
      string_merge(string_merge("sourcep", file_number), ".lisp");
  FILE *program_file_write = fopen(program_file_path, "w");
  int results = fputs(program, program_file_write);
  if (results == EOF) {
    puts("error writing program file");
    return 1;
  }
  fclose(program_file_write);

  // execute the file
  FILE *program_file_read = fopen(program_file_path, "r");
  if (!program_file_read) {
    puts("error reading program file");
    return 1;
  }
  cell *res = NULL;
  cell *env = memory->global_env;
  jmp_destination = setjmp(env_buf);
  if (had_error()) {
    puts("error processing program");
    return 1;
  }
  while (!feof(program_file_read) && fpeek(program_file_read)) {
    cell *sexpr = read_sexpr(program_file_read);
    res = eval(sexpr, env);
  }
  fclose(program_file_read);
  printf("%i ", res->value);

  // write the raw result to a file
  char *result_file_path =
      string_merge(string_merge("resultp", file_number), ".lisp");
  FILE *result_file_write = fopen(result_file_path, "w");
  int r1 = fputs(result, result_file_write);
  if (r1 == EOF) {
    puts("error writing result file");
    return 1;
  }
  fclose(result_file_write);

  // read the raw result
  FILE *result_file_read = fopen(result_file_path, "r");
  if (!result_file_read) {
    puts("error reading result file");
    return 1;
  }
  cell *expected_result = read_sexpr(result_file_read);
  fclose(result_file_read);
  int ret = (total_eq(expected_result, res));
  free_pi();
  return 0;
}
