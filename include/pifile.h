/** @defgroup pifile
 *
 * @brief Provides file handling
 *
 */

/** @addtogroup pifile */
/*@{*/
#ifndef PIFILE_H
#define PIFILE_H
#include "pilisp.h"
#include <stdio.h>

void write_compiler_expression_to_file(char * file_name, cell * to_compilate);
void write_program_to_file(char *file_name, char *program_text);
cell *parse_file(char *file_path);

#endif // !PI_FILE
       /*@}*/