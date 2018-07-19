#include <stdio.h>
main(){
  float r=7.125;
  printf(" %9.3f\n",-r);
  printf(" %9.3f\n",r);
  printf(" %-9.3f\n",r);
  printf(" %09.3f\n",-r);
  printf(" %09.3f\n",r);
  printf(" %-09.3f\n",r);
  return 0;
}
