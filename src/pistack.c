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
  for (int i = 0; i < nargs; i++) {
    stack_pointer--;
    if (i == 0) {
      last_created = head = mk_cons(stack[stack_base + i], NULL);
    } else {
      last_created->cdr =  mk_cons(stack[stack_base + i], NULL);
      last_created = last_created->cdr;
    }
  }
  stack_push(head);
}