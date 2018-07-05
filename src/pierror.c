#include "pierror.h"

/**
 * @brief last error occurred
 *
 */
static int last_error = NO_ERROR;

void pi_error(int CODE, char *message) {
  printf(ANSI_COLOR_LIGHT_RED ":(" ANSI_COLOR_RESET " %s\n", message);
  last_error = CODE;
  // free(message);
  cell_space_destroy_stack(memory);
  longjmp(env_buf, jmp_destination); // jumps to the last saved destination
}

void pi_lisp_error(char *message) { pi_error(LISP_ERROR, message); }

void pi_error_few_args() { pi_error(LISP_ERROR, "too few arguments"); }

void pi_error_many_args() { pi_error(LISP_ERROR, "too many arguments"); }

void check_two_args(const cell *args) {
  if (!args || !cdr(args))
    pi_error_few_args();
  if (cddr(args))
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