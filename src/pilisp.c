#include "pilisp.h"

int prompt() {
  while (1) {
    printf("%s", PROMPT_STRING);
    char* line = malloc(sizeof(char) * MAX_LINE_SIZE);
    if(fgets(line,MAX_LINE_SIZE,stdin)){
      printf("> You wrote %s",line);
    } else {
      return 1;
    }
  }
  return 0;
}

int parse_file(char* file_name){

}
