#include "piinit.h"

void init_builtin_lambdas() {
  builtin_lambda_index = 0;
  symbol_car = mk_builtin_lambda("CAR");
  symbol_cdr = mk_builtin_lambda("CDR");
  symbol_cons = mk_builtin_lambda("CONS");
  symbol_atom = mk_builtin_lambda("ATOM");
  symbol_eq = mk_builtin_lambda("EQ");
  symbol_eq_math = mk_builtin_lambda("=");
  symbol_true = mk_builtin_lambda("T");
  symbol_addition = mk_builtin_lambda("+");
  symbol_subtraction = mk_builtin_lambda("-");
  symbol_multiplication = mk_builtin_lambda("*");
  symbol_division = mk_builtin_lambda("/");
  symbol_set = mk_builtin_lambda("SET");
  symbol_lambda = mk_builtin_lambda("LAMBDA");
  symbol_label = mk_builtin_lambda("LABEL");
  symbol_quote = mk_builtin_lambda("QUOTE");
  symbol_cond = mk_builtin_lambda("COND");
  symbol_load = mk_builtin_lambda("LOAD");
  symbol_timer = mk_builtin_lambda("TIME");
  symbol_or = mk_builtin_lambda("OR");
  symbol_and = mk_builtin_lambda("AND");
  symbol_not = mk_builtin_lambda("NOT");
  symbol_greater = mk_builtin_lambda(">");
  symbol_greater_equal = mk_builtin_lambda(">=");
  symbol_less = mk_builtin_lambda("<");
  symbol_less_equal = mk_builtin_lambda("<=");
  symbol_length = mk_builtin_lambda("LENGTH");
  symbol_member = mk_builtin_lambda("MEMBER");
  symbol_nth = mk_builtin_lambda("NTH");
  symbol_file_ended = mk_builtin_lambda("FEOF");
  symbol_env = mk_builtin_lambda("ENV");
  symbol_mem_dump = mk_builtin_lambda("MD");
  symbol_collect_garbage = mk_builtin_lambda("CG");
  symbol_dotimes = mk_builtin_lambda("DOTIMES");
  symbol_list = mk_builtin_lambda("LIST");
  symbol_bye = mk_builtin_lambda("BYE");
  symbol_macro = mk_builtin_lambda("MACRO");
}

// adds to the list of the language symbols the new sym
static void add_language_symbol(cell *sym) {
  // cell *head = mk_cons(sym, LANGUAGE_SYMBOLS);
  // LANGUAGE_SYMBOLS = head;
}

void init_env() {

  memory->global_env = mk_cons(mk_cons(mk_sym("p"), mk_str("a.lisp")), NULL);
  memory->global_env = mk_cons(mk_cons(mk_sym("f"), mk_str("functions.lisp")),
                               memory->global_env);

  // write the basic functions to one file, then load them
  write_program_to_file(
      ".piinit",
      "(set 'setq (macro (name val) (set name val)))"
      "(set 'defun (macro (name param body) "
      "(list 'set (list 'quote name) (list 'lambda param body))))"
      "(setq maze1 '("
      "    (1) "
      "    (0 3)"
      "    (3 -1)"
      "    (1 2) ))"
      ""
      "(defun sm1 (maze actualCell exploredCells doors)"
      "    (cond "
      "        ( (not doors)"
      "            nil ) "
      "        ( t "
      "            (cond "
      "                ( (not (solveMazeRec maze (car doors) exploredCells))"
      "                    (sm1 maze actualCell exploredCells (cdr doors)) ) "
      "                ( t "
      "                    (solveMazeRec maze (car doors) exploredCells)"
      "                ) ) ) ) )"
      ""
      "(defun solveMazeRec "
      "    (maze actualCell exploredCells)"
      "        (cond "
      "            ((= actualCell -1)"
      "                exploredCells"
      "            ) "
      "            ((member actualCell exploredCells)     "
      "                nil)"
      "            (t"
      "                (sm1 maze actualCell (cons actualCell exploredCells) "
      "(nth actualCell maze))"
      "            ) ) )"
      ""
      ""
      "(defun solveMaze "
      "    (maze)"
      " (solveMazeRec maze 0 '()) )");
  parse_file(".piinit");
}

void init_pi() {
  init_builtin_lambdas();
  init_memory();
  init_env();
}

cell *load_env(char *init_file_path) {
  FILE *fp = fopen("init.lisp", "r");
  if (fp == NULL) {
    printf("No init file found\n");
    return NULL;
  }
  return read_sexpr(fp);
}

void free_pi() { free_memory(); }