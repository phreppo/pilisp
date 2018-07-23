#include "pistack.h"

// EVERYTHING IS UNSAFE!

void stack_push(cell *c) {
  stack[stack_pointer] = c;
  stack_pointer++;
}

cell *stack_pop() {
  cell *ret = stack[stack_pointer - 1];
  stack_pointer--;
  return ret;
}

void stack_car(size_t stack_base, unsigned char nargs) {
  stack_pointer--;
  stack_push(stack[stack_base]->car);
}

void stack_cdr(size_t stack_base, unsigned char nargs) {
  stack_pointer--;
  stack_push(stack[stack_base]->cdr);
}

void stack_list(size_t stack_base, unsigned char nargs) {
  cell *head = NULL;
  cell *last_created = NULL;
  size_t i = 0;
  for (i = 0; i < nargs; i++) {
    // stack_pointer--;
    if (i == 0) {
      last_created = head = mk_cons(stack[stack_base + i], NULL);
    } else {
      last_created->cdr = mk_cons(stack[stack_base + i], NULL);
      last_created = last_created->cdr;
    }
  }
  stack_pointer -= nargs;
  stack_push(head);
}

cell * asm_call_with_stack_base(cell *args, size_t stack_base) {
  cell *machine_code_cell = args->car;
  args = args->cdr;
#if CHECKS
  if (!is_str(machine_code_cell))
    pi_lisp_error("first arg of asm must be a machine code string");
#endif
  char *machine_code = machine_code_cell->str;
  size_t i = 0;
  size_t initial_stack_pointer = stack_pointer;
  char instruction;
  for (i = 0; i < strlen(machine_code); i++) {
    instruction = machine_code[i];
    switch (instruction) {

    case '!':
      // load const
      stack_push(args->car);
      args = args->cdr;
      break;

    case '@':
      // load from stack
      // have to pass a pointer base
      break;

    case '$':
      // call builtin stack
      stack_push(args->car);
      // get the next machine code: it will be the number of params
      unsigned char nargs = (unsigned char)machine_code[i + 1] - 'A';
      i++;
      cell *fun = args->car;
      args = args->cdr;
      stack_pointer--;
      fun->bs(stack_pointer - nargs, nargs);
      break;

    default:
      pi_lisp_error("unknown machine code");
      break;
    }
  }
#if CHECKS
  if (stack_pointer > (initial_stack_pointer + 1))
    pi_lisp_error("stack error: there's something left on the stack");
  if (stack_pointer < (initial_stack_pointer + 1))
    pi_lisp_error(
        "stack error: something has removed too much args on the stack");
#endif
  return stack_pop();
}