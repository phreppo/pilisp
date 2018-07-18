#include "pistack.h"

void stack_push(cell * c){
    stack[stack_pointer] = c; 
    stack_pointer++;
}

void stack_pop(){
    stack_pointer--;
}