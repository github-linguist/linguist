#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char *substring(const char *s, size_t n, ptrdiff_t m)
{
  char *result;
  /* check for null s */
  if (NULL == s)
    return NULL;
  /* negative m to mean 'up to the mth char from right' */
  if (m < 0)
    m = strlen(s) + m - n + 1;

  /* n < 0 or m < 0 is invalid */
  if (n < 0 || m < 0)
    return NULL;

  /* make sure string does not end before n
   * and advance the "s" pointer to beginning of substring */
  for ( ; n > 0; s++, n--)
    if (*s == '\0')
      /* string ends before n: invalid */
      return NULL;

  result = malloc(m+1);
  if (NULL == result)
    /* memory allocation failed */
    return NULL;
  result[0]=0;
  strncat(result, s, m); /* strncat() will automatically add null terminator
                          * if string ends early or after reading m characters */
  return result;
}

char *str_wholeless1(const char *s)
{
  return substring(s, 0, strlen(s) - 1);
}

char *str_fromch(const char *s, int ch, ptrdiff_t m)
{
  return substring(s, strchr(s, ch) - s, m);
}

char *str_fromstr(const char *s, char *in, ptrdiff_t m)
{
  return substring(s, strstr(s, in) - s , m);
}


#define TEST(A) do {		\
    char *r = (A);		\
    if (NULL == r)		\
      puts("--error--");	\
    else {			\
      puts(r);			\
      free(r);			\
    }				\
  } while(0)

int main()
{
  const char *s = "hello world shortest program";

  TEST( substring(s, 12, 5) );		// get "short"
  TEST( substring(s, 6, -1) );		// get "world shortest program"
  TEST( str_wholeless1(s) );		// "... progra"
  TEST( str_fromch(s, 'w', 5) );	// "world"
  TEST( str_fromstr(s, "ro", 3) );	// "rog"

  return 0;
}
