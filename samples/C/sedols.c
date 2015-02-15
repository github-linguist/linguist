#include <stdio.h>
#include <ctype.h>
#include <string.h>

int sedol_weights[] = {1, 3, 1, 7, 3, 9};
const char *reject = "AEIOUaeiou";

int sedol_checksum(const char *sedol6)
{
  int len = strlen(sedol6);
  int sum = 0, i;

  if ( len == 7 ) {
    fprintf(stderr, "SEDOL code already checksummed? (%s)\n", sedol6);
    return sedol6[6] & 0x7f;
  }
  if ( (len > 7) || (len < 6) || ( strcspn(sedol6, reject) != 6 )) {
    fprintf(stderr, "not a SEDOL code? (%s)\n", sedol6);
    return -1;
  }
  for(i=0; i < 6; i++) {
    if ( isdigit(sedol6[i]) ) {
      sum += (sedol6[i]-'0')*sedol_weights[i];
    } else if ( isalpha(sedol6[i]) ) {
      sum += ((toupper(sedol6[i])-'A') + 10)*sedol_weights[i];
    } else {
      fprintf(stderr, "SEDOL with not alphanumeric digit\n");
      return -1;
    }
  }
  return (10 - (sum%10))%10 + '0';
}


#define MAXLINELEN 10
int main()
{
  char line[MAXLINELEN];
  int sr, len;
  while( fgets(line, MAXLINELEN, stdin) != NULL ) {
    len = strlen(line);
    if ( line[len-1] == '\n' ) line[len-1]='\0';
    sr = sedol_checksum(line);
    if ( sr > 0 )
      printf("%s%c\n", line, sr);
  }
  return 0;
}
