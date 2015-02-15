/*30th August, 2012
Abhishek Ghosh*/

#include<stdlib.h>
#include<stdio.h>

int
main ()
{
  int i;
  char *str = getenv ("LANG");

  for (i = 0; str[i + 2] != 00; i++)
    {
      if ((str[i] == 'u' && str[i + 1] == 't' && str[i + 2] == 'f')
          || (str[i] == 'U' && str[i + 1] == 'T' && str[i + 2] == 'F'))
        {
          printf
            ("Unicode is supported on this terminal and U+25B3 is : \u25b3");
          i = -1;
          break;
        }
    }

  if (i != -1)
    printf ("Unicode is not supported on this terminal.");

  return 0;
}
