#include "picell.h"
#include "pilisp.h"
#include <stdio.h>

int fpeek(FILE *const fp) {
  const int c = getc(fp);
  return c == EOF ? 0 : ungetc(c, fp);
}

int main(int argc, char **argv) {
  char *program = argv[1];
  // char * program ="(or NIL NIL NIL NIL)";
  // char *result = "NIL ";
  char *result = argv[2];

  // write the program in a file
  FILE *program_file_write = fopen("sourcep.lisp", "w");
  int results = fputs(program, program_file_write);
  if (results == EOF) {
    puts("error writing program file");
    return 1;
  }
  fclose(program_file_write);

  // execute the file
  FILE *program_file_read = fopen("sourcep.lisp", "r");
  if (!program_file_read) {
    puts("error reading program file");
    return 1;
  }
  init_env();
  cell *res = NULL;
  cell *env = NULL;
  jmp_destination = setjmp(env_buf);
  if (had_error()) {
    puts("error processing program");
    return 1;
  }
  while (!feof(program_file_read) && fpeek(program_file_read)) {
    cell *sexpr = read_sexpr(program_file_read);
    res = eval(sexpr, &env);
  }
  fclose(program_file_read);
  // printf("%i ", res->value);

  // write the raw result to a file
  FILE *result_file_write = fopen("resultp.lisp", "w");
  int r1 = fputs(result, result_file_write);
  if (r1 == EOF) {
    puts("error writing result file");
    return 1;
  }
  fclose(result_file_write);

  // read the raw result
  FILE *result_file_read = fopen("resultp.lisp", "r");
  if (!result_file_read) {
    puts("error reading result file");
    return 1;
  }
  cell *expected_result = read_sexpr(result_file_read);
  close(result_file_read);
  return !(total_eq(expected_result, res));
 }
