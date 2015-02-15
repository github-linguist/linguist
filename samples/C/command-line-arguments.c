#include <stdlib.h>
#include <stdio.h>

int main(int argc, char* argv[])
{
  int i;
  (void) printf("This program is named %s.\n", argv[0]);
  for (i = 1; i < argc; ++i)
    (void) printf("the argument #%d is %s\n", i, argv[i]);
  return EXIT_SUCCESS;
}
