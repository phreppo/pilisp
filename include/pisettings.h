/** @defgroup pisesttings
 *
 * @brief Provides definitions for pilisp settings
 *
 */

/** @addtogroup pisettings */
/*@{*/

#ifndef PISETTINGS_H
#define PISETTINGS_H

/********************************************************************************
 *                               GENERAL SETTINGS
 ********************************************************************************/

// WARNING: performances = 1 is unsafe and startup of pilisp and parsing is
// really slow
#define PERFORMANCES 0

#if PERFORMANCES

// dangerous, will cause segfault
#define EXTREME_PERF 0

// many functions will be declared inline, however the code won't compile in
// many compilers and the builtd directory generated with meson must be
// generated with: meson build -Dc_args=-Og. The compiler flag -O3 soulh be set
// to be correctly compiled. Use only for testing performances
#define INLINE_FUNCTIONS 1

#endif

// 0 => the memory will be dirty => segfault
// anyway a good amount of programs could run anyway
// in the middle between gc and no gc. 1 suggested.
#define DEEP_REMOVE 1

/********************************************************************************
 *                              MEMORY SETTINGS
 ********************************************************************************/

// size of the first created block of cells: must be greater than the init
// size, or will fail tests
#if PERFORMANCES
#if EXTREME_PERF
#define INITIAL_BLOCK_SIZE 134217728
#else
#define INITIAL_BLOCK_SIZE 40000
#endif
#else
#define INITIAL_BLOCK_SIZE 8
#endif

// number of blocks initially allocated
#define INITIAL_BLOCKS 10000

// (n_free_cells/n_tot_cells) <= NEW_BLOCK_THRESHOLD => allocate a new
// block
#define NEW_BLOCK_THRESHOLD 0.8

// WARNING: if 0 the memory will always be dirty
#if PERFORMANCES
#if EXTREME_PERF
#define COLLECT_GARBAGE 0
#else
#define COLLECT_GARBAGE 1
#endif
#else
#define COLLECT_GARBAGE 1
#endif

#define STACK_LIMIT 10000

/********************************************************************************
 *                                 DEBUGGING
 ********************************************************************************/

// in (md) prints free cells
#define PRINT_FREE_CELLS 0

#define PRINT_ONLY_DANGLING_CELLS 0

// describes what appens evaluating sexpressions
#define DEBUG_EVAL_MODE 0

// gives an output when pushing or removing things in the stack
#define DEBUG_PUSH_REMOVE_MODE 0

// prints the env while in debug eval mode
#define DEBUG_EVAL_PRINT_ENV_MODE 0

// when performing gargabe collection prints messages
#define DEBUG_GARBAGE_COLLECTOR_MODE 0

/********************************************************************************
 *                                   INIT
 ********************************************************************************/

// 0 => no checks about types nor errors, just segfaults => use ONLY when
// testing performances on correct programs
#if PERFORMANCES
#define CHECKS 0
#else
#define CHECKS 1
#endif

/********************************************************************************
 *                                   COMPILING SETTIGNS
 ********************************************************************************/

#define PI_COMPILE_FILE_NAME_PREFIX ".picompile"

#define PI_COMPILER_FILE_NAME_PREFIX ".picompiler"

#define REMOVE_TMP_FILES 1

/********************************************************************************
 *                                  LIMITS
 ********************************************************************************/

// max length of a token
#define MAX_TOK_LEN 512

// max length of a error message
#define ERROR_MESSAGE_LEN 512

// EXACT number of builtin lambdas
#define N_BUILTIN_LAMBDA 50

// EXACT number of builtin lambdas
#define N_BUILTIN_MACRO 50

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
