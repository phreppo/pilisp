#include "pilisp.h"

int pi_prompt() {

  // printf("\n  ____  ____  __    ____  ___  ____ \n (  _ \\(_  _)(  )  (_  _)/
  // __)(  _ \\\n  )___/ _)(_  )(__  _)(_ \\__ \\ )___/\n (__)
  // (____)(____)(____)(___/(__) \n\n");

  // printf(COLOR1 "\n\t0000000000000000000000000000000\n\t00" COLOR2
  //               "         _ _ _             " COLOR1 "00\n\t00" COLOR2
  //               "        (_) (_)            " COLOR1 "00\n\t00" COLOR2
  //               "   _ __  _| |_ ___ _ __    " COLOR1 "00\n\t00" COLOR2
  //               "  | '_ \\| | | / __| '_ \\   " COLOR1 "00\n\t00" COLOR2
  //               "  | |_) | | | \\__ \\ |_) |  " COLOR1 "00\n\t00" COLOR2
  //               "  | .__/|_|_|_|___/ .__/   " COLOR1 "00\n\t00" COLOR2
  //               "  | |             | |      " COLOR1 "00\n\t00" COLOR2
  //               "  |_|             |_|      " COLOR1 "00\n\t00" COLOR2
  //               "                           " COLOR1
  //               "00\n\t0000000000000000000000000000000 \n\n"
  //               ANSI_COLOR_RESET);

  printf(
      COLOR1
      "\n"
      "===============================================================\n" COLOR2
          COLOR1 "" COLOR2
      "\t             88  88  88                          \n" COLOR1 "" COLOR2
      "\t             \"\"  88  \"\"                          \n" COLOR1
      "" COLOR2 "\t                 88                              \n" COLOR1
      "" COLOR2 "\t8b,dPPYba,   88  88  88  ,adPPYba,  8b,dPPYba,   \n" COLOR1
      "" COLOR2
      "\t88P'    \"8a  88  88  88  I8[    \"\"  88P'    \"8a  \n" COLOR1
      "" COLOR2 "\t88       d8  88  88  88   `\"Y8ba,   88       d8  \n" COLOR1
      "" COLOR2 "\t88b,   ,a8\"  88  88  88  aa    ]8I  88b,   ,a8\"  \n" COLOR1
      "" COLOR2
      "\t88`YbbdP\"'   88  88  88  `\"YbbdP\"'  88`YbbdP\"'   \n" COLOR1
      "" COLOR2 "\t88                                  88           \n" COLOR1
      "" COLOR2 "\t88                                  88           \n" COLOR1
      "===============================================================\n\n"

      ANSI_COLOR_RESET);

  bool repeat = true;
  while (repeat) {
    // sets the destination for longjump here if errors were encountered
    // during parsing
    jmp_destination = setjmp(env_buf);
    if (had_error()) {
      // skip line: ()))) will print just one error
      reset_error();
      char ch;
      if (!scanf("%c", &ch))
        pi_lisp_error("failed to read char");
      while (ch != '\n')
        if (!scanf("%c", &ch))
          pi_lisp_error("failed to read char");
    }
    printf(ANSI_COLOR_BLUE "-> " ANSI_COLOR_RESET);
    cell *sexpression = read_sexpr(stdin);
    cell *result = eval(sexpression, memory->global_env);
    printf(ANSI_COLOR_GREEN "   " ANSI_COLOR_RESET);
    print_sexpr(result);
    puts("");
    if (result == symbol_bye)
      repeat = false;
    else
      cell_remove_recursive(result);
  }
  return 0;
}

int pi_parse_args_files(int argc, char **argv) {
  jmp_destination = setjmp(env_buf);
  if (had_error()) {
    exit(1);
  }
  // parse one or more files
  unsigned long i = 1;
  for (i = 1; i < argc; i++) {
    cell *res = parse_file(argv[i]);
    print_sexpr(res); // for every file prints the last result
    puts("");
  }
  return 0;
}