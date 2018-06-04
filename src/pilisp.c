#include "pilisp.h"

int prompt() {
  while (1) {
    printf("%s", PROMPT_STRING);
    char *line = malloc(sizeof(char) * MAX_LINE_SIZE);
    if (fgets(line, MAX_LINE_SIZE, stdin)) {
      printf("> You wrote %s", line);
    } else {
      return 1;
    }
  }
  return 0;
}

int interpret_file(char *file_name) {
  FILE *pf;
  pf = fopen(file_name, "r");
  if (pf == NULL) {
    perror("Error opening input file");
    fprintf(stderr, "Error opening file: %s\n", strerror(errno));
  } else {
    fclose(pf);
  }
}
