#include "pilisp.h"

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

  bool repeat = true;
  while (repeat) {
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
    cell *sexpression = read_sexpr(stdin);
    cell *result = eval(sexpression, memory->global_env);
    printf(ANSI_COLOR_GREEN ":) " ANSI_COLOR_RESET);
    print_sexpr(result);
    puts("");
    if (result == symbol_bye) 
      repeat = false;
  }
  return 0;
}