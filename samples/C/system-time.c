#include<time.h>
#include<stdio.h>
#include<stdlib.h>
int main(){
  time_t my_time = time(NULL);
  printf("%s", ctime(&my_time));
  return 0;
}
