#include "pistack.h"
#include "picore.h"

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

void empty_stack(){
  stack_pointer = stack;
}

void stack_car(size_t stack_base, unsigned char nargs) {
  stack_pointer--;
  if (stack[stack_base]) {
    cell *res = stack[stack_base]->car;
    cell_remove(stack[stack_base]);
    cell_remove_recursive(stack[stack_base]->cdr);
    stack_push(res);
  } else {
    stack_push(NULL);
  }
}

void stack_cdr(size_t stack_base, unsigned char nargs) {
  stack_pointer--;
  if (stack[stack_base]) {
    cell *res = stack[stack_base]->cdr;
    cell_remove(stack[stack_base]);
    cell_remove_recursive(stack[stack_base]->car);
    stack_push(res);
  } else {
    stack_push(NULL);
  }
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

cell *asm_call_with_stack_base(cell *args, cell *env, size_t stack_base) {
  cell *machine_code_cell = args->car;
  cell *initial_args = args;
  args = args->cdr;
#if CHECKS
  if (!is_str(machine_code_cell))
    pi_lisp_error("first arg of asm must be a machine code string");
#endif
  char *machine_code = machine_code_cell->str;
  size_t i = 0;
  size_t initial_stack_pointer = stack_pointer;
  char instruction;
  unsigned char nargs;
  cell *mutable_cell; // why this? beacuse first statement in a switch can' t be
                      // a declaration, so we need to declare this first when we
                      // would need a tmp var in the switch
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
      nargs = (unsigned char)machine_code[i + 1] - 'A';
      i++;
      size_t cell_index_in_stack = stack_base + nargs;
      cell *val = stack[cell_index_in_stack]; // calcola dove sta nello stack
      stack_push(val);
      break;

    case '$':
      // call builtin stack
      // get the next machine code: it will be the number of params
      nargs = (unsigned char)machine_code[i + 1] - 'A';
      i++;
      cell *fun = args->car;
      args = args->cdr;
      fun->bs(stack_pointer - nargs, nargs);
      break;

    case '?':
      // extern name
      mutable_cell = assoc(args->car,env);
      stack_push( mutable_cell ? mutable_cell->cdr : NULL);
      args = args->cdr;
      break;

    default:
      pi_lisp_error("unknown machine code");
      break;
    }
  }
#if CHECKS
  if (stack_pointer > (initial_stack_pointer + 1))
    pi_error_stack_overflow();
  if (stack_pointer < (initial_stack_pointer + 1))
    pi_error_stack_undeflow();
#endif
  unsafe_cell_remove(machine_code_cell);
  cell_remove_args(initial_args);
  return stack_pop();
}

void stack_cons(size_t stack_base, unsigned char nargs){
  
}
void stack_atom(size_t stack_base, unsigned char nargs){
  
}
void stack_eq(size_t stack_base, unsigned char nargs){
  
}
void stack_addition(size_t stack_base, unsigned char nargs){
  
}
