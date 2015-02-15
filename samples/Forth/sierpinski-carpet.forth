\ Generates a square Sierpinski gasket
: 1? over 3 mod 1 = ;                  ( n1 n2 -- n1 n2 f)
: 3/ 3 / swap ;                        ( n1 n2 -- n2/3 n1)
                                       \ is this cell in the carpet?
: incarpet                             ( n1 n2 -- f)
  begin over over or while 1? 1? and if 2drop false exit then 3/ 3/ repeat
  2drop true                           \ return true if in the carpet
;
                                       \ draw a carpet of n size
: carpet                               ( n --)
  1 swap 0 ?do 3 * loop dup            \ calculate power of 3
  0 ?do dup 0 ?do i j incarpet if [char] # else bl then emit loop cr loop
  drop                                 \ evaluate every cell in the carpet
;

cr 4 carpet
