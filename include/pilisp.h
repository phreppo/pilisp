/** @defgroup pilisp
 *
 * @brief Links the other modules of pilisp
 *
 */

/** @addtogroup pilisp */
/*@{*/
#ifndef PILISP_h
#define PILISP_h
#define PROMPT_STRING "pi>"
#include "pibuiltin.h"
#include "picell.h"
#include "piinit.h"
#include "picore.h"
#include "pierror.h"
#include "piparser.h"
#include "piprint.h"
#include <ctype.h>
#include <errno.h>
#include <error.h>
#include <setjmp.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define ANSI_COLOR_BLACK            "\x1b[0;30m"
#define ANSI_COLOR_BLUE             "\x1b[0;34m"
#define ANSI_COLOR_GREEN            "\x1b[0;32m"
#define ANSI_COLOR_CYAN             "\x1b[0;36m"
#define ANSI_COLOR_RED              "\x1b[0;31m"
#define ANSI_COLOR_PURPLE           "\x1b[0;35m"
#define ANSI_COLOR_BROWN            "\x1b[0;33m"
#define ANSI_COLOR_GRAY             "\x1b[0;37m"
#define ANSI_COLOR_DARK_GRAY        "\x1b[1;30m"
#define ANSI_COLOR_LIGHT_BLUE       "\x1b[1;34m"
#define ANSI_COLOR_LIGHT_GREEN      "\x1b[1;32m"
#define ANSI_COLOR_LIGHT_CYAN       "\x1b[1;36m"
#define ANSI_COLOR_LIGHT_RED        "\x1b[1;31m"
#define ANSI_COLOR_LIGHT_PURPLE     "\x1b[1;35m"
#define ANSI_COLOR_YELLOW           "\x1b[1;33m"
#define ANSI_COLOR_WHITE            "\x1b[1;37m"
#define ANSI_COLOR_RESET   "\x1b[0m"
#define COLOR1 ANSI_COLOR_LIGHT_BLUE
#define COLOR2 ANSI_COLOR_YELLOW
#define DEBUG_MEM_MODE 0

int jmp_destination;
jmp_buf env_buf;

/**
 * @brief Displays pilisp prompt
 *
 * @return int 0 if no error occurred
 */
int pi_prompt();
cell * parse_file(char * file_path);

#endif // !PILISP_h

/*@}*/
