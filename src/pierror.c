#include "pierror.h"

void pi_error(int CODE, const char* message){
    printf("%s ERROR: %s\n",PROMPT_STRING,message);
    exit(1);
}
