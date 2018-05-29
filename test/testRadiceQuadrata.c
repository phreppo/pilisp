#include "pilisp.h"
#include <stdlib.h>

/**
 * @addtogroup Tests
 */
int main(int argc, char *argv[]){
    tipo * s = NULL;
    s = NULL;
    s = malloc(sizeof(tipo));
    s->i = 1;
    double numero = atof(argv[1]);
    double radice = atof(argv[2]);
    free(s);
    return !(radiceQuadrata(numero) == radice);
}