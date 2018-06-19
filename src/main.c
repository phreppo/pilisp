#include "pilisp.h"
#include "pitestutils.h" // TODO: remove on production

int main(int argc, char **argv) { 
    init_env();
    // FILE * f = fopen("/home/phreppo/pilisp/test/expressions/atom.l","r");
    // lexer_file(f);
    // lexer_prompt();
    // parse_prompt();
    eval_prompt();
    return 0;
}