16 constant w
 8 constant h

: rows    w * 2* ;
1 rows constant row
h rows constant size

create world size allot
world   value old
old w + value new

: init   world size erase ;
: age    new old to new to old ;

: foreachrow ( xt -- )
  size 0 do  I over execute  row +loop drop ;

0 constant EMPTY
1 constant          HEAD
2 constant                    TAIL
3 constant                              WIRE
create cstate bl c, char H c, char t c, char . c,

: showrow ( i -- ) cr
  old + w over + swap do I c@ cstate + c@ emit loop ;
: show  ['] showrow foreachrow  ;


: line ( row addr len -- )
  bounds do
    i c@
    case
    bl of EMPTY over c! endof
    'H of HEAD  over c! endof
    't of TAIL  over c! endof
    '. of WIRE  over c! endof
    endcase
    1+
  loop drop ;

: load ( filename -- )
  r/o open-file throw
  init  old row + 1+  ( file row )
  begin  over pad 80 rot read-line throw
  while  over pad rot line
         row +
  repeat
  2drop close-file throw
  show cr ;


: +head ( sum i -- sum )
  old + c@ HEAD = if 1+ then ;
: conductor ( i WIRE -- i HEAD|WIRE )
  drop 0
  over 1- row - +head
  over    row - +head
  over 1+ row - +head
  over 1-       +head
  over 1+       +head
  over 1- row + +head
  over    row + +head
  over 1+ row + +head
  1 3 within if HEAD else WIRE then ;

\ before:          empty    head   tail   wire

create transition  ' noop , ' 1+ , ' 1+ , ' conductor ,

\ after:           empty    tail   wire   head|wire

: new-state ( i -- )
  dup  old + c@
  dup cells transition + @ execute
  swap new + c! ;

: newrow ( i -- )
  w over + swap do I new-state loop ;
: gen  ['] newrow foreachrow  age ;

: wireworld begin gen 0 0 at-xy show key? until ;
