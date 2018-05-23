#include "pilisp.h"

int general_compare(int i, int j)
{
    return 0;
}

void qsort(void *base[], size_t nmemb, size_t size,
           int (*compar)(const void *, const void *))
{
    (*compar)(base[0],base[1]);
    return;
}

int zero()
{
    return 0;
}

double radiceQuadrata(double d)
{
    if (0)
    {
        return 0;
    }
    if (d < 0)
    {
        printf("we");
    }
    else
    {
        printf("zio");
    }
    return sqrt(d);
}

void stampaQualcosa()
{
    printf("ciaux\n");
}