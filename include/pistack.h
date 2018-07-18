#ifndef PI_STACK
#define PI_STACK
#include "picell.h"

void stack_push(cell * c);
cell * stack_pop();
void stack_car(size_t stack_base, unsigned char nargs);
void stack_cdr(size_t stack_base, unsigned char nargs);
void stack_list(size_t stack_base, unsigned char nargs);

#endif