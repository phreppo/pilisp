#include "pilisp.h"

// TODO: make this run
int pi_prompt() {

  // printf("\n  ____  ____  __    ____  ___  ____ \n (  _ \\(_  _)(  )  (_  _)/
  // __)(  _ \\\n  )___/ _)(_  )(__  _)(_ \\__ \\ )___/\n (__)
  // (____)(____)(____)(___/(__) \n\n");

  printf(COLOR1 "\n\t0000000000000000000000000000000\n\t00" COLOR2
                "         _ _ _             " COLOR1 "00\n\t00" COLOR2
                "        (_) (_)            " COLOR1 "00\n\t00" COLOR2
                "   _ __  _| |_ ___ _ __    " COLOR1 "00\n\t00" COLOR2
                "  | '_ \\| | | / __| '_ \\   " COLOR1 "00\n\t00" COLOR2
                "  | |_) | | | \\__ \\ |_) |  " COLOR1 "00\n\t00" COLOR2
                "  | .__/|_|_|_|___/ .__/   " COLOR1 "00\n\t00" COLOR2
                "  | |             | |      " COLOR1 "00\n\t00" COLOR2
                "  |_|             |_|      " COLOR1 "00\n\t00" COLOR2
                "                           " COLOR1
                "00\n\t0000000000000000000000000000000 \n\n" ANSI_COLOR_RESET);

  // cell *env = load_env(INIT_FILE_PATH_GLOBAL);
  // printf("env: ");
  // print_sexpr(env);
  // puts("");
  while (1) {
    // sets the destination for longjump here if errors were encountered
    // during parsing
    jmp_destination = setjmp(env_buf);
    if (had_error()) {
      // skip line: ()))) will print just one error
      reset_error();
      char ch;
      scanf("%c", &ch);
      while (ch != '\n')
        scanf("%c", &ch);
    }
    printf(ANSI_COLOR_BLUE " > " ANSI_COLOR_RESET);
    cell *result = eval(read_sexpr(stdin), &GLOBAL_ENV);
    printf(ANSI_COLOR_GREEN ":) " ANSI_COLOR_RESET);
    print_sexpr(result);
    puts("");
    // printf("env: ");
    // print_sexpr(GLOBAL_ENV);
    // puts("");
  }
  return 0;
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
    if (sexpr != symbol_file_ended) 
      res = eval(sexpr, &GLOBAL_ENV);
  }
  fclose(program_file);
  return res;
}