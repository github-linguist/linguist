: f+! ( f addr -- ) dup f@ f+ f! ;
: ,f0s ( n -- ) falign 0 do 0e f, loop ;

: period @ ;
: used cell+ ;
: head 2 cells + ;
: sum  3 cells + faligned ;
: ring ( addr -- faddr )
  dup sum float+ swap head @ floats + ;

: update ( fvalue addr -- addr )
       dup ring f@ fnegate dup sum f+!
  fdup dup ring f!         dup sum f+!
  dup head @ 1+  over period mod  over head ! ;

: moving-average
  create ( period -- ) dup , 0 , 0 , 1+ ,f0s
  does>  ( fvalue -- avg )
    update
    dup used @ over period < if 1 over used +! then
    dup sum f@ used @ 0 d>f f/ ;

3 moving-average sma
1e sma f.  \ 1.
2e sma f.  \ 1.5
3e sma f.  \ 2.
4e sma f.  \ 3.
