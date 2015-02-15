#include <stdio.h>
#include <string.h>

const char *haystack[] = {
  "Zig", "Zag", "Wally", "Ronald", "Bush", "Krusty", "Charlie",
  "Bush", "Boz", "Zag", NULL
};

int search_needle(const char *needle, const char **hs)
{
  int i = 0;
  while( hs[i] != NULL ) {
    if ( strcmp(hs[i], needle) == 0 ) return i;
    i++;
  }
  return -1;
}

int search_last_needle(const char *needle, const char **hs)
{
  int i, last=0;
  i = last = search_needle(needle, hs);
  if ( last < 0 ) return -1;
  while( hs[++i] != NULL ) {
    if ( strcmp(needle, hs[i]) == 0 ) {
      last = i;
    }
  }
  return last;
}

int main()
{
  printf("Bush is at %d\n", search_needle("Bush", haystack));
  if ( search_needle("Washington", haystack) == -1 )
    printf("Washington is not in the haystack\n");
  printf("First index for Zag: %d\n", search_needle("Zag", haystack));
  printf("Last index for Zag: %d\n", search_last_needle("Zag", haystack));
  return 0;
}
