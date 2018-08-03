/** @defgroup pisettings
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

/**
 * @brief 1 will imply better performances: about 4 times faster programs
 * execution, but you will lose checks (then segmentation fault will appear if
 * programs aren't correct) and the size of executable will grow because some
 * small functions will be inlined
 *
 */
#define PERFORMANCES 0

#if PERFORMANCES

/**
 * @brief Disables garbage collection. This will make programs run faster but
 * after some time a segmentation fault will occur due to memory leaks
 *
 */
#define EXTREME_PERF 0

/**
 * @brief
 *  many functions will be declared inline, however the code won't compile in
 * many compilers and the build directory generated with meson must be
 * generated with: meson build -Dc_args=-Og. The compiler flag -O3 should be set
 * to be correctly compiled. Use only for testing performances
 *
 */
#define INLINE_FUNCTIONS 1

#endif

/**
 * @brief 0 => the memory will be dirty => segfault
 * anyway a good amount of programs could run anyway
 * in the middle between gc and no gc. 1 suggested.
 *
 */
#define DEEP_REMOVE 1

/********************************************************************************
 *                              MEMORY SETTINGS
 ********************************************************************************/

#if PERFORMANCES
#if EXTREME_PERF
/**
 * @brief Size of the first created block in the memory
 *
 */
#define INITIAL_BLOCK_SIZE 134217728
#else
/**
 * @brief Size of the first created block in the memory
 *
 */
#define INITIAL_BLOCK_SIZE 65536
#endif
#else
/**
 * @brief Size of the first created block in the memory
 *
 */
#define INITIAL_BLOCK_SIZE 8
#endif

/**
 * @brief Dimension of the array pointing to cell blocks
 *
 */
#define INITIAL_BLOCKS 10000

/**
 * @brief (n_free_cells/n_tot_cells) <= NEW_BLOCK_THRESHOLD => allocate a new
 * block
 *
 */
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

/**
 * @brief Max dimension of the stack
 *
 */
#define STACK_LIMIT 10000

/********************************************************************************
 *                                 DEBUGGING
 ********************************************************************************/

/**
 * @brief Print free cells in the (md) function
 *
 */
#define PRINT_FREE_CELLS 1

/**
 * @brief Prints only the dangling cells in the (md) command . Use for debug
 * purposes
 *
 */
#define PRINT_ONLY_DANGLING_CELLS 1

/**
 * @brief If set to 1 will display debug informations while running the gc
 *
 */
#define DEBUG_GARBAGE_COLLECTOR_MODE 0

/********************************************************************************
 *                                   INIT
 ********************************************************************************/

// 0 => no checks about types nor errors, just segfaults => use ONLY when
// testing performances on correct programs
#if PERFORMANCES
/**
 * @brief If set to 0 will remove the checks about errors during the execution
 * of a program. This will make run faster correct programs.
 *
 */
#define CHECKS 0
#else
/**
 * @brief If set to 0 will remove the checks about errors during the execution
 * of a program. This will make run faster correct programs.
 *
 */
#define CHECKS 1
#endif

/********************************************************************************
 *                                   FILES SETTINGS
 ********************************************************************************/

/**
 * @brief Prefix of the temporary file used to compile one expression
 *
 */
#define PI_COMPILE_FILE_NAME_PREFIX ".picompile"

/**
 * @brief Prefix of the temporary file used to load the Pilisp compiler
 *
 */
#define PI_COMPILER_FILE_NAME_PREFIX ".picompiler"

/**
 * @brief If set to 1 will remove temporary files. 1 suggested.
 *
 */
#define REMOVE_TMP_FILES 1

/********************************************************************************
 *                                  LIMITS
 ********************************************************************************/

/**
 * @brief Max length of a LISP token
 *
 */
#define MAX_TOK_LEN 512

/**
 * @brief Max length of a error message
 *
 */
#define ERROR_MESSAGE_LEN 512

/**
 * @brief Number of builtin lambdas. It's used to create the array to store
 * them.
 *
 */
#define N_BUILTIN_LAMBDA 50

/**
 * @brief Number of builtin macros. It's used to create the array to store them.
 *
 */
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
