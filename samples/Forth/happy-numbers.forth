: next ( n -- n )
  0 swap begin 10 /mod >r  dup * +  r> ?dup 0= until ;

: cycle? ( n -- ? )
  here dup @ cells +
  begin dup here >
  while 2dup @ = if 2drop true exit then
        1 cells -
  repeat
  1 over +!  dup @ cells + !  false ;

: happy? ( n -- ? )
  0 here !  begin next dup cycle? until  1 = ;

: happy-numbers ( n -- )
  0 swap 0 do
    begin 1+ dup happy? until dup .
  loop drop ;

8 happy-numbers  \ 1 7 10 13 19 23 28 31
