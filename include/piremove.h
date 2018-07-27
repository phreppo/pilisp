#ifndef PIREMOVE_H
#define PIREMOVE_H
#include "picell.h"
#include "picore.h"

void cell_remove_lambda(cell * new_env, cell * old_env, cell * args, cell * fn);
void cell_remove_label(cell * new_env, cell * fn);
void cell_remove_eval_macro(cell *new_env, cell *old_env, cell *expression);
void cell_remove_apply_macro(cell * env, cell *old_env, cell *args, cell * fn);
void cell_remove_let_param(cell *params);

#endif // !PIREMOVE_H