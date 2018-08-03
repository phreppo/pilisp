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
  symbol_quote = mk_builtin_macro("QUOTE", quote);
  symbol_setq = mk_builtin_macro("SETQ", setq);
  symbol_let = mk_builtin_macro("LET", let);
  symbol_defun = mk_builtin_macro("DEFUN", defun);
  symbol_timer = mk_builtin_macro("TIME", timer);
  symbol_map = mk_builtin_macro("MAP", map);
  symbol_load = mk_builtin_macro("LOAD", load);
  symbol_dotimes = mk_builtin_macro("DOTIMES", dotimes);
  symbol_cond = mk_builtin_macro("COND", cond);
  symbol_asm = mk_builtin_macro("ASM", asm_call);
  symbol_compile = mk_builtin_macro("COMPILE", compile);
}

void init_builtin_lambdas() {
  builtin_lambdas_index = 0;
  symbol_car = mk_builtin_lambda("CAR", builtin_car, stack_car);
  symbol_cdr = mk_builtin_lambda("CDR", builtin_cdr, stack_cdr);
  symbol_cons = mk_builtin_lambda("CONS", builtin_cons, stack_cons);
  symbol_atom = mk_builtin_lambda("ATOM", builtin_atom, stack_atom);
  symbol_eq = mk_builtin_lambda("EQ", builtin_eq, stack_eq);
  symbol_eq_math = mk_builtin_lambda("=", builtin_eq, NULL);
  symbol_true = mk_builtin_lambda("T", NULL, NULL);
  symbol_addition = mk_builtin_lambda("+", addition, stack_addition);
  symbol_subtraction = mk_builtin_lambda("-", subtraction, NULL);
  symbol_multiplication = mk_builtin_lambda("*", multiplication, NULL);
  symbol_division = mk_builtin_lambda("/", division, NULL);
  symbol_set = mk_builtin_lambda("SET", set, NULL);
  symbol_lambda = mk_builtin_lambda("LAMBDA", NULL, NULL);
  symbol_label = mk_builtin_lambda("LABEL", NULL, NULL);
  symbol_or = mk_builtin_lambda("OR", or, NULL);
  symbol_and = mk_builtin_lambda("AND", and, NULL);
  symbol_not = mk_builtin_lambda("NOT", not, NULL);
  symbol_greater = mk_builtin_lambda(">", greater, NULL);
  symbol_greater_equal = mk_builtin_lambda(">=", greater_eq, NULL);
  symbol_less = mk_builtin_lambda("<", less, NULL);
  symbol_less_equal = mk_builtin_lambda("<=", less_eq, NULL);
  symbol_length = mk_builtin_lambda("LENGTH", length, NULL);
  symbol_member = mk_builtin_lambda("MEMBER", member, NULL);
  symbol_nth = mk_builtin_lambda("NTH", nth, NULL);
  symbol_list = mk_builtin_lambda("LIST", list, stack_list);
  symbol_subseq = mk_builtin_lambda("SUBSEQ", subseq, NULL);
  symbol_reverse = mk_builtin_lambda("REVERSE", reverse, NULL);
  symbol_file_ended = mk_builtin_lambda("FEOF", NULL, NULL);
  symbol_env = mk_builtin_lambda("ENV", env, NULL);
  symbol_mem_dump = mk_builtin_lambda("MD", mem_dump, NULL);
  symbol_collect_garbage = mk_builtin_lambda("CG", collect_garbage_call, NULL);
  symbol_bye = mk_builtin_lambda("BYE", bye, NULL);
  symbol_macro = mk_builtin_lambda("MACRO", NULL, NULL);
  symbol_integerp = mk_builtin_lambda("INTEGERP", integerp, NULL);
  symbol_symbolp = mk_builtin_lambda("SYMBOLP", symbolp, NULL);
  symbol_write = mk_builtin_lambda("WRITE", write, NULL);
  symbol_concatenate = mk_builtin_lambda("CONCATENATE", concatenate, NULL);
  symbol_append = mk_builtin_lambda("APPEND", append, NULL);
  symbol_lasm = mk_builtin_lambda("LASM", NULL, NULL);
  symbol_string = mk_builtin_lambda("STRING", NULL, NULL);
}

void init_stack() { stack_pointer = 0; }

void init_env() {
  memory->global_env = NULL;
  // write the basic functions to one file, then load them
  write_program_to_file(
      ".piinit",

      "(set 'defmacro (macro (name param body) "
      "(list 'set (list 'quote name) (list 'macro param body))))"

      "(defun 1+ (num) (+ num 1))"

      "(defun 1- (num) (- num 1))"

      "(defun id (x) x)"

      "(defun null (arg) (not arg))"

      "(setq diff  \"./test/lisp_programs/diff.lisp\")"

      "(setq cdiff \"./test/lisp_programs/compilable_diff.lisp\")"

      "(setq b \"./examples/bench.lisp\")"

      "(setq f \"./examples/functions.lisp\")"
  );
  
  parse_file(".piinit");
#if REMOVE_TMP_FILES
  remove(".piinit");
#endif
  char *compiler_tmp_file_name = generate_pi_compiler_tmp_file_name();
  write_compiler_to_file(compiler_tmp_file_name);
  parse_file(compiler_tmp_file_name);
#if REMOVE_TMP_FILES
  remove(compiler_tmp_file_name);
#endif
  collect_garbage(memory);
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

char *get_compiler_source_hardcoded() {
  return ""
         "(defun plc (not_evaluated_expression)       "
         "( get_interpretable_code        "
         "( _compile not_evaluated_expression nil)       "
         "not_evaluated_expression))       "
         "       "
         "(setq builtin_stack_lambdas        "
         "'( car cdr cons atom eq list +))       "
         "       "
         "(defun _compile (expr symbol_table)       "
         "(cond        "
         "((atom expr)        "
         "(list ( compile_atom expr symbol_table)))       "
         "       "
         "(( is_quoted_expression expr)       "
         "(list ( compile_quote expr)))       "
         "       "
         "((atom (car expr))       "
         "( compile_atom_function expr symbol_table))        "
         "       "
         "(( else)       "
         ":notcompilable )))       "
         "       "
         "(defun compile_atom (ato symbol_table)       "
         "(cond       "
         "(( null ato)       "
         "(cons :loadconst ato))       "
         "((eq ato t)       "
         "(cons :loadconst ato))       "
         "(( has_value_in_stack ato symbol_table)       "
         "(cons :loadstack ( get_stack_index ato symbol_table)))       "
         "((symbolp ato)       "
         "(cons :loadsymbol ato))        "
         "(( else)                     "
         "(cons :loadconst ato))))       "
         "       "
         "(defun has_value_in_stack (name symbol_table)       "
         "(cond        "
         "((null symbol_table) nil)       "
         "((eq name ( extract_first_symbol symbol_table)) t)       "
         "(( else) ( has_value_in_stack name ( next symbol_table)))))       "
         "       "
         "(defun get_stack_index (name symbol_table)       "
         "(cond        "
         "((null symbol_table) nil)       "
         "((eq name ( extract_first_symbol symbol_table))       "
         "( extract_first_index symbol_table))       "
         "(( else)       "
         "( get_stack_index name ( next symbol_table)))))       "
         "       "
         "(defun extract_first_symbol (symbol_table)       "
         "(car (car symbol_table)))       "
         "       "
         "(defun extract_first_index (symbol_table)       "
         "(cdr (car symbol_table)))       "
         "       "
         "(defun compile_quote (quote_expression)       "
         "(cons :loadconst ( extract_cons_cell quote_expression)))       "
         "       "
         "(defun extract_cons_cell (quote_expression)       "
         "(car (cdr quote_expression)))       "
         "       "
         "(defun compile_atom_function (expr symbol_table)       "
         "( compile_atom_function_name_args (car expr) (cdr expr)        "
         "symbol_table))       "
         "       "
         "(defun compile_atom_function_name_args (fun args symbol_table)       "
         "(cond        "
         "(( is_builtin_stack fun)        "
         "( compile_builtin_stack fun args symbol_table))       "
         "(( is_lambda fun)       "
         "( compile_lambda fun args symbol_table))       "
         "(( else)        "
         ":notcompilable )))       "
         "       "
         "(defun is_builtin_stack (fun)       "
         "(member fun builtin_stack_lambdas))       "
         "       "
         "(defun is_lambda (fun)       "
         "(eq fun 'lambda))       "
         "       "
         "(defun compile_builtin_stack (fun args_list symbol_table)       "
         "( compile_args_and_append_builtin_stack fun args_list ( count_args   "
         "     "
         "args_list) symbol_table))       "
         "       "
         "(defun compile_args_and_append_builtin_stack (fun args_list        "
         "initial_args_number symbol_table)       "
         "(cond        "
         "((null args_list)        "
         "( create_builtin_stack_trailer fun initial_args_number))       "
         "(( else)         "
         "( let       "
         "((first_arg_compiled        "
         "( compile_first_arg args_list symbol_table))       "
         "(rest_of_the_args_compiled        "
         "( compile_remaining_list_and_append_builtin_stack fun args_list      "
         "  "
         "initial_args_number symbol_table)))       "
         "( compile_only_if_everything_is_compilable first_arg_compiled        "
         "rest_of_the_args_compiled)))))       "
         "       "
         "(defun compile_only_if_everything_is_compilable (first_arg_compiled  "
         "      "
         "rest_of_the_args_compiled)       "
         "(cond        "
         "(( both_compilables first_arg_compiled rest_of_the_args_compiled)    "
         "   "
         "(append first_arg_compiled rest_of_the_args_compiled))       "
         "(( else)       "
         ":notcompilable)))       "
         "       "
         "(defun compile_first_arg (args_list symbol_table)       "
         "( _compile (car args_list) symbol_table))       "
         "       "
         "(defun compile_remaining_list_and_append_builtin_stack (fun        "
         "args_list initial_args_number symbol_table)       "
         "( compile_args_and_append_builtin_stack fun ( next args_list)        "
         "initial_args_number symbol_table))       "
         "       "
         "(defun create_builtin_stack_trailer (fun initial_args_number)       "
         "(list        "
         "(cons :cbs fun)       "
         "( get_params_trailer initial_args_number)))       "
         "       "
         "(defun get_params_trailer (args_number)       "
         "(cons :argsnum args_number))       "
         "       "
         "(defun compile_lambda (fun args symbol_table)       "
         "(let (       "
         "(lambda_args  ( extract_lambda_args args))       "
         "(lambda_body  ( extract_lambda_body args))       "
         "(new_symbol_table ( build_symbol_table ( extract_lambda_args args)   "
         "     "
         "symbol_table)))       "
         "(let (       "
         "(lambda_args_number_instruction (        "
         "build_lambda_args_number_instruction lambda_args))       "
         "(lambda_body_instruction_list ( build_lambda_body_instruction_list   "
         "     "
         "lambda_body new_symbol_table)))        "
         "( compile_lambda_only_if_compilable lambda_args_number_instruction   "
         "     "
         "lambda_body_instruction_list))))       "
         "       "
         "(defun compile_lambda_only_if_compilable        "
         "(lambda_args_number_instruction lambda_body_instructions_list)       "
         "(cond        "
         "(( is_compilable lambda_body_instructions_list)       "
         "(cons       "
         "lambda_args_number_instruction       "
         "lambda_body_instructions_list))       "
         "(( else)       "
         ":notcompilable)))       "
         "       "
         "(defun build_lambda_body_instruction_list (lambda_body symbol_table) "
         "       "
         "( _compile lambda_body symbol_table))       "
         "       "
         "(defun build_symbol_table (lambda_args old_symbol_table)       "
         "(let        "
         "((new_symbol_table_head        "
         "(reverse ( build_symbol_table_with_position lambda_args 0))))        "
         "(append        "
         "new_symbol_table_head       "
         "old_symbol_table)))       "
         "       "
         "(defun build_symbol_table_with_position (lambda_args "
         "actual_position)       "
         "(cond        "
         "((null lambda_args) nil)       "
         "(( else) ( build_one_symbol_and_the_rest_of_the_list lambda_args     "
         "   "
         "actual_position))))       "
         "       "
         "(defun build_one_symbol_and_the_rest_of_the_list (lambda_args        "
         "actual_position)       "
         "(cons        "
         "( build_one_symbol lambda_args actual_position)       "
         "( build_symbol_table_with_position ( next lambda_args) (1+        "
         "actual_position))))       "
         "       "
         "(defun build_one_symbol (lambda_args actual_position)       "
         "(cons (car lambda_args) actual_position))       "
         "       "
         "(defun build_lambda_args_number_instruction (lambda_args)       "
         "(cons :lambdanargs ( count_args lambda_args)))       "
         "       "
         "(defun extract_lambda_args (lambda_cons)       "
         "(car lambda_cons))       "
         "       "
         "(defun extract_lambda_body (lambda_cons)       "
         "(car (cdr lambda_cons)))       "
         "       "
         "(defun get_interpretable_code (compiled_expression        "
         "original_expression)       "
         "(cond        "
         "((eq compiled_expression :notcompilable)        "
         "original_expression )       "
         "((not ( is_lasm compiled_expression))       "
         "(cons 'asm        "
         "( build_interpretable_string_and_args compiled_expression)))       "
         "(( else)       "
         "(cons 'lasm       "
         "(cons        "
         "(cdr (car compiled_expression))       "
         "( build_interpretable_string_and_args ( next        "
         "compiled_expression)))))))       "
         "       "
         "(defun is_lasm (compiled_expression)       "
         "(eq :lambdanargs (car (car compiled_expression))))       "
         "       "
         "(defun build_interpretable_string_and_args (compiled_expression)     "
         "  "
         "(cons        "
         "( extract_machine_code_string compiled_expression )        "
         "( extract_args compiled_expression)))       "
         "       "
         "(defun extract_instruction_code (compiled_expression)       "
         "(car (car compiled_expression)))       "
         "       "
         "(defun extract_arg (compiled_expression)       "
         "(cdr (car compiled_expression)))       "
         "       "
         "(defun extract_args (compiled_expression)       "
         "(cond       "
         "((null compiled_expression) nil)       "
         "(( else) ( build_one_arg_and_extract_next compiled_expression))))    "
         "   "
         "       "
         "(defun build_one_arg_and_extract_next (compiled_expression)       "
         "(cond       "
         "(( must_ignore_arg compiled_expression)       "
         "( extract_args (cdr compiled_expression)))       "
         "(( else)       "
         "(cons        "
         "( extract_arg compiled_expression)        "
         "( extract_args (cdr compiled_expression))))))       "
         "       "
         "(defun must_ignore_arg (compiled_expression)       "
         "(cond       "
         "((eq :argsnum ( extract_instruction_code compiled_expression)) t)    "
         "   "
         "((eq :loadstack ( extract_instruction_code compiled_expression)) t)  "
         "     "
         "(( else) nil)))       "
         "       "
         "(defun extract_machine_code_string (compiled_expression)       "
         "(cond        "
         "((null compiled_expression) \"\")       "
         "(( else) ( build_remaining_machine_code_string_char        "
         "compiled_expression))))       "
         "       "
         "(defun build_remaining_machine_code_string_char "
         "(compiled_expression)       "
         "(concatenate 'string        "
         "( get_instruction_code compiled_expression)        "
         "( extract_machine_code_string ( next compiled_expression))))       "
         "       "
         "(defun get_instruction_code (compiled_expression)       "
         "( translate_instruction_code        "
         "( extract_instruction_code compiled_expression)       "
         "( extract_arg compiled_expression)))       "
         "       "
         "(defun translate_instruction_code (code arg)       "
         "(cond        "
         "((eq code :loadconst) \"!\")       "
         "((eq code :loadsymbol) \"?\")       "
         "((eq code :cbs) \"$\")       "
         "((eq code :loadstack) ( get_instruction_code_for_stack_load arg))    "
         "    "
         "((eq code :argsnum) ( translate_num_to_digit arg))       "
         "(( else) \"__ERROR:UNKNOWN_INSTRUCTION_CODE__\")))       "
         "       "
         "(defun get_instruction_code_for_stack_load (stack_index)       "
         "(concatenate 'string \"@\" ( translate_num_to_digit stack_index)))   "
         "    "
         "       "
         "(defun translate_num_to_digit (args_number)       "
         "(cond       "
         "((eq args_number 0) \"A\")       "
         "((eq args_number 1) \"B\")       "
         "((eq args_number 2) \"C\")       "
         "((eq args_number 3) \"D\")       "
         "((eq args_number 4) \"E\")       "
         "((eq args_number 5) \"F\")       "
         "((eq args_number 6) \"G\")       "
         "((eq args_number 7) \"H\")       "
         "((eq args_number 8) \"I\")       "
         "((eq args_number 9) \"J\")       "
         "((eq args_number 10) \"K\")       "
         "((eq args_number 11) \"L\")       "
         "((eq args_number 12) \"M\")       "
         "((eq args_number 13) \"n\")       "
         "((eq args_number 14) \"O\")       "
         "((eq args_number 15) \"P\")       "
         "((eq args_number 16) \"Q\")       "
         "((eq args_number 17) \"R\")       "
         "((eq args_number 18) \"S\")       "
         "((eq args_number 19) \"T\")       "
         "((eq args_number 20) \"U\")       "
         "((eq args_number 21) \"V\")       "
         "((eq args_number 22) \"W\")       "
         "((eq args_number 23) \"X\")       "
         "((eq args_number 24) \"Y\")       "
         "((eq args_number 25) \"Z\")       "
         "(( else) \"__ERROR:TOO_MANY_ARGS__\")))       "
         "       "
         "(defun is_quoted_expression (expr)       "
         "(and (atom (car expr)) (eq 'quote (car expr))))       "
         "       "
         "(defun is_compilable (expression)       "
         "(not (eq expression :notcompilable)))       "
         "       "
         "(defun count_args (args_list)        "
         "(length args_list))       "
         "       "
         "(defun else () t)       "
         "       "
         "(defun next (l) (cdr l))       "
         "       "
         "(defun both_compilables (first_sequence second_sequence)       "
         "(and ( is_compilable first_sequence)        "
         "( is_compilable second_sequence)))       "
         "       "
         "T";
}