#include <stdio.h>

int main()
{
   FILE *lp;
   lp = fopen("/dev/lp0","w");
   fprintf(lp,"Hello world!\n");
   fclose(lp);
   return 0;
}
