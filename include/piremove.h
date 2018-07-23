#ifndef PIREMOVE_H
#define PIREMOVE_H
#include "picell.h"
#include "picore.h"

void cell_remove_lambda(cell * new_env, cell * old_env, cell * args, cell * fn);
void cell_remove_macro(cell *new_env, cell *old_env, cell *expression);


#endif // !PIREMOVE_H