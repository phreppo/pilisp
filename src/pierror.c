#include "pierror.h"

void pi_error(int CODE, const char* message){
    printf("%s ERROR: %s\n",PROMPT_STRING,message);
    last_error = CODE;
    longjmp(env_buf,jmp_destination);
    // exit(1);
}

int get_last_error(){
    return last_error;
}

void reset_error(){
    last_error = NO_ERROR;
}

bool had_error(){
    return last_error != NO_ERROR;
}