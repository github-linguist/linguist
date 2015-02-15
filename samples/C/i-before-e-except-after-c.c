%{
  /*
    compilation and example on a GNU linux system:

    $ flex --case-insensitive --noyywrap --outfile=cia.c source.l
    $ make LOADLIBES=-lfl cia
    $ ./cia < unixdict.txt
    I before E when not preceded by C: plausible
    E before I when preceded by C: implausible
    Overall, the rule is: implausible
  */
  int cie, cei, ie, ei;
%}

%%

cie ++cie, ++ie; /* longer patterns are matched preferentially, consuming input */
cei ++cei, ++ei;
ie ++ie;
ei ++ei;
.|\n ;

%%

int main() {
  cie = cei = ie = ei = 0;
  yylex();
  printf("%s: %s\n","I before E when not preceded by C", (2*ei < ie ? "plausible" : "implausible"));
  printf("%s: %s\n","E before I when preceded by C", (2*cie < cei ? "plausible" : "implausible"));
  printf("%s: %s\n","Overall, the rule is", (2*(cie+ei) < (cei+ie) ? "plausible" : "implausible"));
  return 0;
}
