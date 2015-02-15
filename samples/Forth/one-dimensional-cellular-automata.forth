: init ( bits count -- )
  0 do dup 1 and c, 2/ loop drop ;

20 constant size
create state $2556e size init 0 c,

: .state
  cr size 0 do
    state i + c@ if ." #" else space then
  loop ;

: ctable create does> + c@ ;
ctable rules $68 8 init

: gen
  state c@ ( window )
  size 0 do
    2*  state i + 1+ c@ or  7 and
    dup rules state i + c!
  loop drop ;

: life1d ( n -- )
  .state 1 do gen .state loop ;

10 life1d
