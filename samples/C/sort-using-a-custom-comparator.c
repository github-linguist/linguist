#include <stdlib.h>  /* for qsort */
#include <string.h>  /* for strlen */
#include <strings.h> /* for strcasecmp */

int mycmp(const void *s1, const void *s2)
{
    const char *l = *(const char **)s1, *r = *(const char **)s2;
    size_t ll = strlen(l), lr = strlen(r);

    if (ll > lr) return -1;
    if (ll < lr) return 1;
    return strcasecmp(l, r);
}

int main()
{
    const char *strings[] = {
      "Here", "are", "some", "sample", "strings", "to", "be", "sorted" };

    qsort(strings, sizeof(strings)/sizeof(*strings), sizeof(*strings), mycmp);
    return 0;
}
