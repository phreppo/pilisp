#include "piinit.h"

void init_builtin_lambdas() {
  builtin_lambdas_index = 0;
  symbol_car = mk_builtin_lambda("CAR",builtin_car);
  symbol_cdr = mk_builtin_lambda("CDR",builtin_cdr);
  symbol_cons = mk_builtin_lambda("CONS",builtin_cons);
  symbol_atom = mk_builtin_lambda("ATOM",builtin_atom);
  symbol_eq = mk_builtin_lambda("EQ",builtin_eq);
  symbol_eq_math = mk_builtin_lambda("=",builtin_eq);
  symbol_true = mk_builtin_lambda("T",symbol_true);
  symbol_addition = mk_builtin_lambda("+",addition);
  symbol_subtraction = mk_builtin_lambda("-",subtraction);
  symbol_multiplication = mk_builtin_lambda("*",multiplication);
  symbol_division = mk_builtin_lambda("/",division);
  symbol_set = mk_builtin_lambda("SET",set);
  symbol_lambda = mk_builtin_lambda("LAMBDA",NULL);
  symbol_label = mk_builtin_lambda("LABEL",NULL);
  symbol_quote = mk_builtin_lambda("QUOTE",NULL);
  symbol_cond = mk_builtin_lambda("COND",NULL);
  symbol_or = mk_builtin_lambda("OR",or);
  symbol_and = mk_builtin_lambda("AND",and);
  symbol_not = mk_builtin_lambda("NOT",not);
  symbol_greater = mk_builtin_lambda(">",greater);
  symbol_greater_equal = mk_builtin_lambda(">=",greater_eq);
  symbol_less = mk_builtin_lambda("<",less);
  symbol_less_equal = mk_builtin_lambda("<=",less_eq);
  symbol_length = mk_builtin_lambda("LENGTH",length);
  symbol_member = mk_builtin_lambda("MEMBER",member);
  symbol_nth = mk_builtin_lambda("NTH",nth);
  symbol_list = mk_builtin_lambda("LIST",list);
  symbol_subseq = mk_builtin_lambda("SUBSEQ",subseq);
  symbol_reverse = mk_builtin_lambda("REVERSE",reverse);
  symbol_file_ended = mk_builtin_lambda("FEOF",NULL);
  symbol_env = mk_builtin_lambda("ENV",env);
  symbol_mem_dump = mk_builtin_lambda("MD",mem_dump);
  symbol_collect_garbage = mk_builtin_lambda("CG",collect_garbage_call);
  symbol_dotimes = mk_builtin_lambda("DOTIMES",NULL);
  symbol_bye = mk_builtin_lambda("BYE",bye);
  symbol_macro = mk_builtin_lambda("MACRO",NULL);
  symbol_integerp = mk_builtin_lambda("INTEGERP",integerp);
  symbol_symbolp = mk_builtin_lambda("SYMBOLP",symbolp);
  symbol_write = mk_builtin_lambda("WRITE",write);
}

void init_builtin_macros() {
  builtin_macros_index = 0;
  symbol_setq = mk_builtin_macro("SETQ");
  symbol_let = mk_builtin_macro("LET");
  symbol_defun = mk_builtin_macro("DEFUN");
  symbol_timer = mk_builtin_macro("TIME");
  symbol_map = mk_builtin_macro("MAP");
  symbol_load = mk_builtin_macro("LOAD");
}

void init_env() {
  memory->global_env = NULL;
  // write the basic functions to one file, then load them
  write_program_to_file(
      ".piinit",
      "(set 'defmacro (macro (name param body) " // check this
      "(list 'set (list 'quote name) (list 'macro param body))))"

      "(defun 1+ (num) (+ num 1))"

      "(setq d \"./test/lisp_programs/diff.lisp\")"

      "(setq p \"./examples/a.lisp\")"

      "(setq f \"./examples/functions.lisp\")");
  parse_file(".piinit");
  cell_space_destroy_stack(memory); // remove thrash
  collect_garbage(memory);
}

void init_pi() {
  init_builtin_macros();
  init_builtin_lambdas();
  init_memory();
  init_env();
}

void free_pi() {
  free_memory();
  free_builtin_symbols();
}

void free_builtin_symbols() {
  size_t i = 0;
  for (i = 0; i < builtin_lambdas_index; i++)
    free_cell_pointed_memory(&BUILTIN_LAMBDAS[i]);
  for (i = 0; i < builtin_macros_index; i++)
    free_cell_pointed_memory(&BUILTIN_MACROS[i]);
}