#include "piinit.h"

void init_pi() {
  init_builtin_macros();
  init_builtin_lambdas();
  init_memory();
  init_env();
  init_symbols();
}

void init_builtin_macros() {
  builtin_macros_index = 0;
  symbol_quote = mk_builtin_macro("QUOTE",quote);
  symbol_setq = mk_builtin_macro("SETQ",setq);
  symbol_let = mk_builtin_macro("LET",let);
  symbol_defun = mk_builtin_macro("DEFUN",defun);
  symbol_timer = mk_builtin_macro("TIME",timer);
  symbol_map = mk_builtin_macro("MAP",map);
  symbol_load = mk_builtin_macro("LOAD",load);
  symbol_dotimes = mk_builtin_macro("DOTIMES",dotimes);
  symbol_cond = mk_builtin_macro("COND",cond);
  symbol_asm = mk_builtin_macro("ASM",asm_call);
}

void init_builtin_lambdas() {
  builtin_lambdas_index = 0;
  symbol_car = mk_builtin_lambda("CAR",builtin_car,stack_car);
  symbol_cdr = mk_builtin_lambda("CDR",builtin_cdr,stack_cdr);
  symbol_cons = mk_builtin_lambda("CONS",builtin_cons,NULL);
  symbol_atom = mk_builtin_lambda("ATOM",builtin_atom,NULL);
  symbol_eq = mk_builtin_lambda("EQ",builtin_eq,NULL);
  symbol_eq_math = mk_builtin_lambda("=",builtin_eq,NULL);
  symbol_true = mk_builtin_lambda("T",symbol_true,NULL);
  symbol_addition = mk_builtin_lambda("+",addition,NULL);
  symbol_subtraction = mk_builtin_lambda("-",subtraction,NULL);
  symbol_multiplication = mk_builtin_lambda("*",multiplication,NULL);
  symbol_division = mk_builtin_lambda("/",division,NULL);
  symbol_set = mk_builtin_lambda("SET",set,NULL);
  symbol_lambda = mk_builtin_lambda("LAMBDA",NULL,NULL);
  symbol_label = mk_builtin_lambda("LABEL",NULL,NULL);
  symbol_or = mk_builtin_lambda("OR",or,NULL);
  symbol_and = mk_builtin_lambda("AND",and,NULL);
  symbol_not = mk_builtin_lambda("NOT",not,NULL);
  symbol_greater = mk_builtin_lambda(">",greater,NULL);
  symbol_greater_equal = mk_builtin_lambda(">=",greater_eq,NULL);
  symbol_less = mk_builtin_lambda("<",less,NULL);
  symbol_less_equal = mk_builtin_lambda("<=",less_eq,NULL);
  symbol_length = mk_builtin_lambda("LENGTH",length,NULL);
  symbol_member = mk_builtin_lambda("MEMBER",member,NULL);
  symbol_nth = mk_builtin_lambda("NTH",nth,NULL);
  symbol_list = mk_builtin_lambda("LIST",list,stack_list);
  symbol_subseq = mk_builtin_lambda("SUBSEQ",subseq,NULL);
  symbol_reverse = mk_builtin_lambda("REVERSE",reverse,NULL);
  symbol_file_ended = mk_builtin_lambda("FEOF",NULL,NULL);
  symbol_env = mk_builtin_lambda("ENV",env,NULL);
  symbol_mem_dump = mk_builtin_lambda("MD",mem_dump,NULL);
  symbol_collect_garbage = mk_builtin_lambda("CG",collect_garbage_call,NULL);
  symbol_bye = mk_builtin_lambda("BYE",bye,NULL);
  symbol_macro = mk_builtin_lambda("MACRO",NULL,NULL);
  symbol_integerp = mk_builtin_lambda("INTEGERP",integerp,NULL);
  symbol_symbolp = mk_builtin_lambda("SYMBOLP",symbolp,NULL);
  symbol_write = mk_builtin_lambda("WRITE",write,NULL);
  symbol_concatenate = mk_builtin_lambda("CONCATENATE",concatenate,NULL);
  symbol_append = mk_builtin_lambda("APPEND",append,NULL);
  symbol_lasm = mk_builtin_lambda("LASM",NULL,NULL);
  symbol_string = mk_builtin_lambda("STRING",NULL,NULL);
  symbol_compile = mk_builtin_lambda("COMPILE",compile,NULL);
}

void init_stack(){
  stack_pointer = 0;
}

void init_env() {
  memory->global_env = NULL;
  // write the basic functions to one file, then load them
  write_program_to_file(
      ".piinit",

      "(set 'defmacro (macro (name param body) " // check this
      "(list 'set (list 'quote name) (list 'macro param body))))"

      "(defun 1+ (num) (+ num 1))"

      "(defun id (x) x)"

      "(setq d \"./test/lisp_programs/diff.lisp\")"

      "(defun null (arg) (not arg))"

      "(setq p \"./examples/a.lisp\")"

      "(setq compiler \"./compiler/compiler.lisp\")"

      "(setq b \"./examples/bench.lisp\")"

      "(setq f \"./examples/functions.lisp\")"

      // "(load compiler)"

      "(setq c compiler)"

      );
  parse_file(".piinit");
  collect_garbage(memory);
}

void init_symbols(){

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