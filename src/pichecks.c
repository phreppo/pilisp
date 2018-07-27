#include "pichecks.h"

/********************************************************************************
 *                                  Arithmetic
 ********************************************************************************/

void check_addition_atom(cell *arg) {
  if (!is_cons(arg))
    pi_lisp_error("impossible to perform addition");
  if (!is_num(car(arg)))
    pi_lisp_error("added a non-number");
}

void check_subtraction(cell *args) {
  if (!args)
    pi_error_few_args();
}

void check_subtraction_atom(cell *arg) {
  if (!is_num(car(arg)))
    pi_lisp_error("subtracted a non-number");
}

void check_multiplication_atom(cell *arg) {
  if (!is_cons(arg))
    pi_lisp_error("impossible to perform multiplication");
  if (!is_num(car(arg)))
    pi_lisp_error("multiplicated a non-number");
}

void check_division(cell *args) {
  if (!args || !cdr(args))
    pi_error_few_args();
  if (!is_num(car(args)) || !is_num(car(cdr(args))))
    pi_lisp_error("divided a non-number");
}

void check_division_atom(cell *arg) {
  if (!is_num(car(arg)))
    pi_lisp_error("divided a non-number");
  if (car(arg)->value == 0)
    pi_lisp_error("division by 0");
}

void check_append(cell *args) {
  check_two_args(args);
  if (car(args) && !is_cons(car(args)))
    pi_lisp_error("first arg must be a list");
}

void check_concatenate(cell *args) {
  check_three_args(args);
  if (!is_sym(car(args)))
    pi_lisp_error("first arg must be a symbol");
  if (!is_str(cadr(args)))
    pi_lisp_error("second arg must be a string");
  if (!is_str(caddr(args)))
    pi_lisp_error("third arg must be a string");
}

/********************************************************************************
 *                                  Comparison
 ********************************************************************************/

void check_comparables(cell *args) {
  check_two_args(args);
  if (!car(args) || !cadr(args)) {
    pi_lisp_error("NIL not allowed as arg");
  }
  if ((car(args) && car(args)->type) != (cadr(args) && cadr(args)->type))
    pi_lisp_error("incompatible types");
}

/********************************************************************************
 *                                  Lists
 ********************************************************************************/

void check_length(cell *args) {
  check_one_arg(args);
  if (car(args) && !is_cons(car(args)) && !is_str(car(args)))
    pi_lisp_error("arg is not a list or a string");
}

void check_member(cell *args) {
  check_two_args(args);
  if (cadr(args) && !is_cons(cadr(args)))
    pi_lisp_error("second arg must be a list");
}

void check_nth(cell *args) {
  check_two_args(args);
  if (!is_num(car(args)))
    pi_lisp_error("first arg must be a number");
  if (cadr(args) && !is_cons(cadr(args)))
    pi_lisp_error("second arg must be a list");
}

void check_subseq(cell*args){
  if(!args || !cdr(args))
    pi_error_few_args();
  // if( caddr(cdr(args)))
    // pi_error_many_args();
  if(!is_str(car(args)))
    pi_lisp_error("first arg in subseq must be a string");

}