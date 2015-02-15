#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char *sconcat(const char *s1, const char *s2)
{
  char *s0 = malloc(strlen(s1)+strlen(s2)+1);
  strcpy(s0, s1);
  strcat(s0, s2);
  return s0;
}

int main()
{
   const char *s = "hello";
   char *s2;

   printf("%s literal\n", s);
   /* or */
   printf("%s%s\n", s, " literal");

   s2 = sconcat(s, " literal");
   puts(s2);
   free(s2);
}
