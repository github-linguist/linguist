variable accumulator
: H cr ." Hello, world!" ;
: Q cr 2dup type ;
: 9 99 verses ;  \ http://rosettacode.org/wiki/99_Bottles_of_Beer#Forth
: + 1 accumulator +! ;

: hq9+ ( "code" -- )
  parse-word 2dup bounds ?do
    i 1 [ get-current literal ] search-wordlist
    if execute else true abort" invalid HQ9+ instruction"
  then loop 2drop ;
