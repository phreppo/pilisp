#include "pilisp.h"

/**
 * @addtogroup Tests
 */
int main(int argc, char *argv[]){
    tipo * s = NULL;
    double numero = atof(argv[1]);
    double radice = atof(argv[2]);
    return !(radiceQuadrata(numero) == radice);
}