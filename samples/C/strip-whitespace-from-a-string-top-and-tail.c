#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

char *rtrim(const char *s)
{
  while( isspace(*s) || !isprint(*s) ) ++s;
  return strdup(s);
}

char *ltrim(const char *s)
{
  char *r = strdup(s);
  if (r != NULL)
  {
    char *fr = r + strlen(s) - 1;
    while( (isspace(*fr) || !isprint(*fr) || *fr == 0) && fr >= r) --fr;
    *++fr = 0;
  }
  return r;
}

char *trim(const char *s)
{
  char *r = rtrim(s);
  char *f = ltrim(r);
  free(r);
  return f;
}

const char *a = "     this is a string      ";

int main()
{
  char *b = rtrim(a);
  char *c = ltrim(a);
  char *d = trim(a);

  printf("'%s'\n'%s'\n'%s'\n", b, c, d);

  free(b);
  free(c);
  free(d);
  return 0;
}
