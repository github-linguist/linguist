#include <stdio.h>

int main(void)
{
  printf("%s\n",
         ( (727 == 0x2d7) &&
           (727 == 01327)    ) ? "true" : "false");

  return 0;
}
