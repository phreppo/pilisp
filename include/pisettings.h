/** @defgroup pisesttings
 *
 * @brief provides definitions for pilisp settings
 *
 */

/** @addtogroup pisettings */
/*@{*/

#ifndef PISETTINGS_H
#define PISETTINGS_H

/********************************************************************************
 *                              MEMORY SETTINGS
 ********************************************************************************/
#define INITIAL_BLOCK_SIZE 2    // size of the first created block of cells
#define INITIAL_BLOCKS 1        // number of blocks initially allocated
#define NEW_BLOCK_THRESHOLD 0.5 //

/********************************************************************************
 *                                 DEBUGGING
 ********************************************************************************/

#define DEBUG_GARBAGE_COLLECTOR_MODE                                           \
  1                       // when performing gargabe collection prints messages
#define DEBUG_EVAL_MODE 1 // describes what appens evaluating sexpressions

/********************************************************************************
 *                                   INIT
 ********************************************************************************/
#define INIT_FILE_PATH_GLOBAL                                                  \
  "../init.lisp" // defines the path to the .lisp init file

/********************************************************************************
 *                                PARSER SETTINGS
 ********************************************************************************/
#define MAX_TOK_LEN 1024 // max length of a token

/********************************************************************************
 *                             CONSOLE ANSI COLORS
 ********************************************************************************/
#define ANSI_COLOR_BLACK "\x1b[0;30m"
#define ANSI_COLOR_BLUE "\x1b[0;34m"
#define ANSI_COLOR_GREEN "\x1b[0;32m"
#define ANSI_COLOR_CYAN "\x1b[0;36m"
#define ANSI_COLOR_RED "\x1b[0;31m"
#define ANSI_COLOR_PURPLE "\x1b[0;35m"
#define ANSI_COLOR_BROWN "\x1b[0;33m"
#define ANSI_COLOR_GRAY "\x1b[0;37m"
#define ANSI_COLOR_DARK_GRAY "\x1b[1;30m"
#define ANSI_COLOR_LIGHT_BLUE "\x1b[1;34m"
#define ANSI_COLOR_LIGHT_GREEN "\x1b[1;32m"
#define ANSI_COLOR_LIGHT_CYAN "\x1b[1;36m"
#define ANSI_COLOR_LIGHT_RED "\x1b[1;31m"
#define ANSI_COLOR_LIGHT_PURPLE "\x1b[1;35m"
#define ANSI_COLOR_YELLOW "\x1b[1;33m"
#define ANSI_COLOR_WHITE "\x1b[1;37m"
#define ANSI_COLOR_RESET "\x1b[0m"
#define COLOR1 ANSI_COLOR_LIGHT_BLUE
#define COLOR2 ANSI_COLOR_YELLOW

#endif // !PISETTINGS_H

/*@}*/
