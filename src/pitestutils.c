#include "pitestutils.h"

void parse_prompt() {
  printf("%s Welcome to the parser prompt, type sexpressions\n", PROMPT_STRING);
  while (1) {
    // sets the destination for longjump here if errors were encountered during
    // parsing
    jmp_destination = setjmp(env_buf);
    if (get_last_error() != NO_ERROR) {
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
  cell * env = memory->global_env;
  printf("env: ");
  print_sexpr(env);
  puts("");
  while (1) {
    // sets the destination for longjump here if errors were encountered during
    // parsing
    jmp_destination = setjmp(env_buf);
    if (get_last_error() != NO_ERROR) {
      reset_error();
    }
    printf("> ");
    cell *list1 = read_sexpr(stdin);
    cell *result = eval(list1, env);
    print_sexpr(result);
    puts("");
  }
}

void pairlis_prompt() {
  printf("%s Welcome to the pairlis prompt, type pairs of sexpressions\n",
         PROMPT_STRING);
  printf("pairlis env: ");
  print_sexpr(memory->global_env);
  puts("");
  cell * env = memory->global_env;
  while (1) {
    // sets the destination for longjump here if errors were encountered during
    // parsing
    jmp_destination = setjmp(env_buf);
    if (get_last_error() != NO_ERROR) {
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