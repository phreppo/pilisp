#include "piutils.h"

char *generate_pi_compile_tmp_file_name() {
  srand(time(NULL));
  int r = rand();

  char str[(int)((ceil(log10(r)) + 1) * sizeof(char))];
  sprintf(str, "%d", r); 
  char *file_name=malloc(strlen(PI_COMPILE_FILE_NAME_PREFIX) + strlen(str) + 1);
  file_name[0] = '\0'; 
  strcat(file_name, PI_COMPILE_FILE_NAME_PREFIX);
  strcat(file_name, str);
}