0 value diag

: south  diag abs + cell+ ;

' cell+ value zig
' south value zag

: init ( n -- )
  1- cells negate to diag
  ['] cell+ to zig
  ['] south to zag ;

: swap-diag   zig zag to zig to zag ;

: put ( n addr -- n+1 addr )
  2dup !  swap 1+ swap ;

: turn ( addr -- addr+E/S )
  zig execute  swap-diag
  diag negate to diag ;

: zigzag ( matrix n -- )
  { n } n init
  0 swap
  n 1 ?do
    put turn
    i 0 do put diag + loop
  loop
  swap-diag
  n 1 ?do
    put turn
    n i 1+ ?do put diag + loop
  loop
  ! ;

: .matrix ( n matrix -- )
  over 0 do
    cr
    over 0 do
      dup @ 3 .r cell+
    loop
  loop 2drop ;

: test ( n -- )  here over zigzag here .matrix ;
5 test
  0  1  5  6 14
  2  4  7 13 15
  3  8 12 16 21
  9 11 17 20 22
 10 18 19 23 24 ok
