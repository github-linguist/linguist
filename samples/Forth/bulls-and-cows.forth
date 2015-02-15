include random.fs

create hidden 4 allot

: ok? ( str -- ? )
  dup 4 <> if 2drop false exit then
  1 9 lshift 1- -rot
  bounds do
    i c@ '1 -
    dup 0 9 within 0= if 2drop false leave then
    1 swap lshift over and
    dup 0= if nip leave then
    xor
  loop 0<> ;

: init
  begin
    hidden 4 bounds do 9 random '1 + i c! loop
    hidden 4 ok?
  until ;

: check? ( addr -- solved? )
  0
  4 0 do
    over i + c@
    4 0 do
      dup hidden i + c@ = if     swap
        i j = if 8 else 1 then + swap
      then
    loop drop
  loop nip
  8 /mod tuck . ." bulls, " . ." cows"
  4 = ;

: guess: ( "1234" -- )
  bl parse 2dup ok? 0= if 2drop ." Bad guess! (4 unique digits, 1-9)" exit then
  drop check? if cr ." You guessed it!" then ;
