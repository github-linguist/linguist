#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#define MAX_BUF 50

int main(void)
{
  char buf[MAX_BUF];
  time_t seconds = time(NULL);
  struct tm *now = localtime(&seconds);
  const char *months[] = {"January", "February", "March", "April", "May", "June",
                          "July", "August", "September", "October", "November", "December"};

  const char *days[] = {"Sunday", "Monday", "Tuesday", "Wednesday","Thursday","Friday","Saturday"};

  (void) printf("%d-%d-%d\n", now->tm_year + 1900, now->tm_mon + 1, now->tm_mday);
  (void) printf("%s, %s %d, %d\n",days[now->tm_wday], months[now->tm_mon],
               now->tm_mday, now->tm_year + 1900);
  /* using the strftime (the result depends on the locale) */
  (void) strftime(buf, MAX_BUF, "%A, %B %e, %Y", now);
  (void) printf("%s\n", buf);
  return EXIT_SUCCESS;
}
