#ifndef PICHECKS_H
#define PICHECKS_H
#include "picell.h"
#include "pierror.h"

void check_append(cell *args);
void check_concatenate(cell *args);

// ==================== Arithmetic checks ====================
void check_addition_atom(cell *arg);
void check_subtraction(cell *args);
void check_subtraction_atom(cell *arg);
void check_multiplication_atom(cell *arg);
void check_division(cell *args);
void check_division_atom(cell *arg);

#endif // !PICHECKS_H