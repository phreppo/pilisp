#include "pistack.h"

// EVERYTHING IS UNSAFE!

void stack_push(cell * c){
    stack[stack_pointer] = c; 
    stack_pointer++;
}

cell* stack_pop(){
    cell * ret = stack[stack_pointer-1]; 
    stack_pointer--;
    return ret;
}

void stack_car(size_t stack_base){
    stack_push(stack[stack_base]->car);
    stack_pointer--;
}

void stack_cdr(size_t stack_base){
    stack_push(stack[stack_base]->cdr);
    stack_pointer--;
}