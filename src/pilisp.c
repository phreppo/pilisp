#include "pilisp.h"

// TODO: make this run
int pi_prompt() {

  // printf("\n  ____  ____  __    ____  ___  ____ \n (  _ \\(_  _)(  )  (_  _)/ __)(  _ \\\n  )___/ _)(_  )(__  _)(_ \\__ \\ )___/\n (__)  (____)(____)(____)(___/(__) \n\n");

  printf("\n\t0000000000000000000000000000000\n\t00         _ _ _             00\n\t00        (_) (_)            00\n\t00   _ __  _| |_ ___ _ __    00\n\t00  | '_ \\| | | / __| '_ \\   00\n\t00  | |_) | | | \\__ \\ |_) |  00\n\t00  | .__/|_|_|_|___/ .__/   00\n\t00  | |             | |      00\n\t00  |_|             |_|      00\n\t00                           00\n\t0000000000000000000000000000000 \n\n");

  cell *env = load_env(INIT_FILE_PATH_GLOBAL);
  printf("env: ");
  print_sexpr(env);
  puts("");
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
    printf("> ");
    cell *result = eval(read_sexpr(stdin), &env);
    print_sexpr(result);
    puts("");
  }
  return 0;
}