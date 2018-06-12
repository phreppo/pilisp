#include "pilisp.h"

// TODO: make this run
int prompt() {
  while (1) {
    printf("%s ", PROMPT_STRING);
    return 0;
  }
  return 0;
}

void pi_message(const char * message){
  printf("%s %s\n",PROMPT_STRING,message);
}
