/*

  David Lambert, 2010-Dec-09

  filter producing morse beep commands.

  build:
    make morse

  use:
    $ echo tie a. | ./morse
    beep -n -f 440 -l 300 -D 100 -n -D 200 -n -f 440 -l 100 -D 100 -n -f 440 -l 100 -D 100 -n -D 200 -n -f 440 -l 100 -D 100 -n -D 200 -n -D 400 -n -f 440 -l 100 -D 100 -n -f 440 -l 300 -D 100 -n -D 200 -n -f 440 -l 100 -D 100 -n -f 440 -l 300 -D 100 -n -f 440 -l 100 -D 100 -n -f 440 -l 300 -D 100 -n -f 440 -l 100 -D 100 -n -f 440 -l 300 -D 100 -n -D 200 -n -D 400 -n -D 400

  bugs:
    What is the space between letter and punctuation?
    Demo truncates input lines at 71 characters or so.

 */

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BIND(A,L,H) ((L)<(A)?(A)<(H)?(A):(H):(L))
/*
  BIND(-1,0,9) is 0
  BIND( 7,0,9) is 7
  BIND(77,0,9) is 9
*/

char
  /* beep args for */
  /* dit  dah     extra space */
  dih[50],dah[50],medium[30],word[30],
  *dd[2] = {dih,dah};
const char
  *ascii = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.,?'!/()&:;=+-_\"$@",
  *itu[] = {
     "13","3111","3131","311","1","1131","331","1111","11","1333","313","1311","33","31","333","1331","3313","131","111","3","113","1113","133","3113","3133","3311","33333","13333","11333","11133","11113","11111","31111","33111","33311","33331","131313","331133","113311","133331","313133","31131","31331","313313","13111","333111","313131","31113","13131","311113","113313","131131","1113113","133131"
  };

void append(char*s,const char*morse) {
  for (; *morse; ++morse)
    strcat(s,dd['3'==*morse]);
  strcat(s,medium);
}

char*translate(const char*i,char*o) {
  const char*pc;
  sprintf(o,"beep");
  for (; *i; ++i)
    if (NULL == (pc = strchr(ascii,toupper(*i))))
      strcat(o,word);
    else
      append(o,itu[pc-ascii]);
  strcat(o,word);
  return o;
}

int main(int ac,char*av[]) {
  char
    sin[73],sout[100000];
  int
    dit = 100;
  if (1 < ac) {
    if (strlen(av[1]) != strspn(av[1],"0123456789"))
      return 0*fprintf(stderr,"use: %s [duration]   dit in ms, default %d\n",*av,dit);
    dit = BIND(atoi(av[1]),1,1000);
  }
  sprintf(dah," -n -f 440 -l %d -D %d",3*dit,dit);
  sprintf(dih," -n -f 440 -l %d -D %d",dit,dit);
  sprintf(medium," -n -D %d",(3-1)*dit);
  sprintf(word," -n -D %d",(7-(3-1)-1)*dit);
  while (NULL != fgets(sin,72,stdin))
    puts(translate(sin,sout));
  return 0;
}
