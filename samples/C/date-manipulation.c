#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main()
{
  struct tm ts;
  time_t t;
  const char *d = "March 7 2009 7:30pm EST";

  strptime(d, "%B %d %Y %I:%M%p %Z", &ts);
  /* ts.tm_hour += 12; instead of t += 12*60*60
     works too. */
  t = mktime(&ts);
  t += 12*60*60;
  printf("%s", ctime(&t));

  return EXIT_SUCCESS;
}
