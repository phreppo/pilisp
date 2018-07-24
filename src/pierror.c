#include "pierror.h"

/**
 * @brief last error occurred
 *
 */
static int last_error = NO_ERROR;

void pi_error(int CODE, char *message) {
  printf(ANSI_COLOR_LIGHT_RED ":(" ANSI_COLOR_RESET " %s\n", message);
  mark(memory->global_env);
  sweep(memory);
  last_error = CODE;
  longjmp(env_buf, jmp_destination); // jumps to the last saved destination
}

void pi_lisp_error(char *message) { pi_error(LISP_ERROR, message); }

void pi_error_few_args() { pi_error(LISP_ERROR, "too few arguments"); }

void pi_error_many_args() { pi_error(LISP_ERROR, "too many arguments"); }

void pi_error_stack() {
  empty_stack();
  pi_lisp_error("stack error: wrong stack pointer");
}

void pi_error_stack_overflow() {
  empty_stack();
  pi_error(LISP_ERROR, "stack error: there's something left on the stack");
}

void pi_error_stack_undeflow() {
  empty_stack();
  pi_error(LISP_ERROR,
           "stack error: something has removed too much args on the stack");
}

void check_two_args(const cell *args) {
  if (!args || !cdr(args))
    pi_error_few_args();
  if (cddr(args))
    pi_error_many_args();
}

void check_three_args(const cell *args) {
  if (!args || !cdr(args) || !cdr(cdr(args)))
    pi_error_few_args();
  if (cdr(cddr(args)))
    pi_error_many_args();
}

void check_one_arg(const cell *args) {
  if (!args)
    pi_error_few_args();
  if (cdr(args))
    pi_error_many_args();
}

int get_last_error() { return last_error; }

void reset_error() { last_error = NO_ERROR; }

bool had_error() { return last_error != NO_ERROR; }