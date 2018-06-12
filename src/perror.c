#include "perror.h"

void pi_error(int CODE, const char* message){
    printf("%sERROR: %s\n",PROMPT_STRING,message);
    exit(1);
}
