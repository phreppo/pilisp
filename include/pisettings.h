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
#define INITIAL_BLOCK_SIZE                                                     \
  10 // size of the first created block of cells: must be greater than the init
    // size, or will fail tests
#define INITIAL_BLOCKS 1 // number of blocks initially allocated
#define NEW_BLOCK_THRESHOLD                                                    \
  0.8 // (n_free_cells/n_tot_cells) <= NEW_BLOCK_THRESHOLD => allocate a new
      // block

/********************************************************************************
 *                                 DEBUGGING
 ********************************************************************************/

#define DEBUG_GARBAGE_COLLECTOR_MODE                                           \
  0                       // when performing gargabe collection prints messages
#define DEBUG_EVAL_MODE 0 // describes what appens evaluating sexpressions
#define DEBUG_PUSH_REMOVE_MODE 0 // when pushing or removing things in the stack

/********************************************************************************
 *                                   INIT
 ********************************************************************************/

#define ERROR_EMPTY_REMOVING                                                   \
  1 // raise an error if trying to remove a cell that is not on the stack
    // (beacuse that cell does not belongs to you)

/********************************************************************************
 *                                  LIMITS
 ********************************************************************************/

#define MAX_TOK_LEN 512      // max length of a token
#define N_BUILTIN_LAMBDA 777 // EXACT number of builtin lambdas
#define N_BUILTIN_MACRO 777 // EXACT number of builtin lambdas

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
