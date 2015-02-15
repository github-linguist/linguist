: .-0 ( n -- n )
  [char] - emit
  dup 10 < if [char] 0 emit then ;

: .short-date
  time&date ( s m h D M Y )
  1 u.r .-0 1 u.r .-0 1 u.r
  drop drop drop ;

: str-table
  create ( n -- ) 0 do , loop
  does>  ( n -- str len ) swap cells + @ count ;

 here ," December"
 here ," November"
 here ," October"
 here ," September"
 here ," August"
 here ," July"
 here ," June"
 here ," May"
 here ," April"
 here ," March"
 here ," February"
 here ," January"
12 str-table months

 here ," Sunday"
 here ," Saturday"
 here ," Friday"
 here ," Thursday"
 here ," Wednesday"
 here ," Tuesday"
 here ," Monday"
7 str-table weekdays

\ Zeller's Congruence
: zeller ( m -- days since March 1 )
  9 + 12 mod 1-   26 10 */ 3 + ;

: weekday ( d m y -- 0..6 )   \ Monday..Sunday
  over 3 < if 1- then
  dup    4 /
  over 100 / -
  over 400 / +  +
  swap zeller + +
  1+ 7 mod ;

: 3dup   dup 2over rot ;

: .long-date
  time&date ( s m h D M Y )
  3dup weekday weekdays type ." , "
  >R 1- months type space 1 u.r ." , " R> .
  drop drop drop ;
