include random.fs

variable flips
create nums 9 cells allot

: .array ( addr len -- ) 0 ?do dup @ . cell+ loop drop ;

: shuffle ( addr len -- )
  2 swap do
    dup i random cells +
    over @ over @  swap
    rot  ! over !
    cell+
  -1 +loop drop ;

: sorted? ( addr len -- )
  1- cells bounds ?do
    i 2@ < if unloop false exit then
  cell +loop true ;

: init
  0 flips !
  nums 10 1 do
    i over !  cell+
  loop drop
  begin nums 9 shuffle nums 9 sorted? 0= until
  cr nums 9 .array ;

: reverse ( addr len -- )
  1- cells bounds
  begin 2dup >
  while over @ over @ >r over ! over r> swap !
        cell+ swap 1 cells - swap
  repeat 2drop ;

: flip ( n -- )
  dup 2 10 within 0= if . ." must be within 2 to 9" exit then
  nums swap reverse
  1 flips +!
  nums 9 sorted? if
    cr ." Got it in " flips @ . ." tries!"
  else
    cr nums 9 .array
  then ;

init
1 2 8 5 7 6 9 4 3  ok
7 flip
9 6 7 5 8 2 1 4 3  ok
