#ifndef PTESTUTILS
#define PTESTUTILS
#include <stdio.h>
#include "pilisp.h"

// TODO: eliminate this on production, function to debug
void lexer_prompt();
void parse_prompt();
int lexer_file(FILE * f);

#endif // !PTESTUTILS