#include "pilisp.h"

int main(int argc, char **argv) {
  if (argc > 1) {
    // has to deal with file
    if(argv > 2){
        // TODO: deal error
        return 1;
    } else {
        // one file
        
    }
  } else {
    // has to display prompt
    return prompt();
  }
}