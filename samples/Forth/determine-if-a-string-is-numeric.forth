: is-numeric ( addr len -- )
  2dup snumber? ?dup if      \ not standard, but >number is more cumbersome to use
   0< if
     -rot type ."  as integer = " .
   else
     2swap type ."  as double = " <# #s #> type
   then
  else 2dup >float if
    type ."  as float = " f.
  else
    type ."  isn't numeric in base " base @ dec.
  then then ;

s" 1234" is-numeric    \ 1234 as integer = 1234
s" 1234." is-numeric    \ 1234. as double = 1234
s" 1234e" is-numeric    \ 1234e as float = 1234.
s" $1234" is-numeric    \ $1234 as integer = 4660  ( hex literal )
s" %1010" is-numeric    \ %1010 as integer = 10  ( binary literal )
s" beef" is-numeric    \ beef isn't numeric in base 10
hex
s" beef" is-numeric    \ beef as integer = BEEF
s" &1234" is-numeric    \ &1234 as integer = 4D2 ( decimal literal )
