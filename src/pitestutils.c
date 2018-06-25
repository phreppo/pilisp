#include "pitestutils.h"

void parse_prompt() {
  printf("%s Welcome to the parser prompt, type sexpressions\n", PROMPT_STRING);
  while (1) {
    // sets the destination for longjump here if errors were encountered during
    // parsing
    jmp_destination = setjmp(env_buf);
    if (get_last_error() != NO_ERROR) {
      // pi_message("you just had an error");
      reset_error();
    } else {
      pi_message("everything was ok with last sexpr");
    }
    cell *root = read_sexpr(stdin);
    printf("lst> ");
    print_sexpr_mode(root, SEXPR_PRINT_DEFAULT);
    puts("");
    printf("ext> ");
    print_sexpr_mode(root, SEXPR_PRINT_VERBOSE);
    puts("");
  }
}

void lexer_prompt() {
  while (1) {
    printf("%s Welcome to the lexer prompt, type tokens\n", PROMPT_STRING);
    int token =
        next_token(stdin); // note: int, not char, required to handle EOF
    while (1) {
      print_token(token);
      token = next_token(stdin);
    }
  }
}

int lexer_file(FILE *f) {
  while (!feof(f))
    read_sexpr(f);
  return 0;
}

void eval_prompt() {
  printf("%s Welcome to the eval prompt, type expressions to be evaluated\n",
         PROMPT_STRING);
  // cell *num1 = mk_num(1);
  // cell *str1 = mk_str("hi");
  // cell *sym1 = mk_sym("a");
  // cell *sym2 = mk_sym("b");
  // cell *env = mk_cons(mk_cons(sym1, num1), mk_cons(mk_cons(sym2, str1),
  // NULL));
  cell *env = load_env(INIT_FILE_PATH_GLOBAL);
  // cell * env = NULL;
  printf("env: ");
  print_sexpr(env);
  puts("");
  while (1) {
    // sets the destination for longjump here if errors were encountered during
    // parsing
    jmp_destination = setjmp(env_buf);
    if (get_last_error() != NO_ERROR) {
      // pi_message("you just had an error");
      reset_error();
    }
    printf("> ");
    cell *list1 = read_sexpr(stdin);
    // printf("sexpr> \t");
    // print_sexpr_mode(list1, SEXPR_PRINT_DEFAULT);
    // puts("");
    cell *result = eval(list1, env);
    print_sexpr(result);
    puts("");
  }
}

void pairlis_prompt() {
  printf("%s Welcome to the pairlis prompt, type pairs of sexpressions\n",
         PROMPT_STRING);
  // cell *num1 = mk_num(1);
  // cell *str1 = mk_str("hi");
  // cell *sym1 = mk_sym("num1");
  // cell *sym2 = mk_sym("str1");
  // cell *env = mk_cons(mk_cons(sym1, num1), mk_cons(mk_cons(sym2, str1),
  // NULL));
  cell *env = load_env(INIT_FILE_PATH_GLOBAL);
  printf("pairlis env: ");
  print_sexpr(env);
  puts("");
  while (1) {
    // sets the destination for longjump here if errors were encountered during
    // parsing
    jmp_destination = setjmp(env_buf);
    if (get_last_error() != NO_ERROR) {
      // pi_message("you just had an error");
      reset_error();
    }
    pi_message("Type labels list: ");
    cell *list1 = read_sexpr(stdin);
    printf("First list> \t");
    print_sexpr_mode(list1, SEXPR_PRINT_DEFAULT);
    puts("");
    pi_message("Type values list: ");
    cell *list2 = read_sexpr(stdin);
    printf("Second list> \t");
    print_sexpr_mode(list2, SEXPR_PRINT_DEFAULT);
    puts("");

    cell *pairl = pairlis(list1, list2, env);
    printf("Pairlis> \t");
    print_sexpr(pairl);
    puts("");
    env = pairl;

    pi_message("Now insert one label");
    cell *label = read_sexpr(stdin);
    cell *pair = assoc(label, env);
    print_sexpr(pair);
    puts("");
  }
}

void mem_prompt() {
  int count = 0;

  while (1) {
    printf("############################## MEMORY "
           "##############################\n");
    print_cell_space(memory);
    printf("> ");
    int i;
    scanf("%i", &i);
    switch (i) {
    case 1:
      mk_num(count++);
      break;

    default:
      break;
    }
  }
}