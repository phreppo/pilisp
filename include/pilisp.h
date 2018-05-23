/** @defgroup Pilisp
 *
 * This module does yada yada yada
 *
 */

/** @addtogroup Pilisp */
/*@{*/
#ifndef PILISP
#define PILISP
#include <math.h>
#include <stdio.h>

/**
 * 
 * @brief Returns zero, nothing more
 * 
 * @return int zero value
 */
int zero();

/**
 * @brief general compare
 * 
 * @param i first number
 * @param j second number
 * @return int always 0
 */
int general_compare(int i,int j);

/**
 * @brief sort alogorithm for generical array
 * 
 * @param base base address
 * @param nmemb number of elements
 * @param size size of the elements
 * @param compar compare function
 */
void qsort(void *base[], size_t nmemb, size_t size, 
           int (*compar)(const void *, const void *));

/**
 * @brief Square root calculator, using math.h function
 * 
 * @param d 
 * @return double 
 */
double radiceQuadrata(double d);

/**
 * @brief Stampa delle cose relativamente utili
 * 
 * Non è molto utile effettivamente
 * 
 */
void stampaQualcosa();

#endif // !PILISP

/*@}*/
