: arshift 0 ?do 2/ loop ;            \ 2/ is an arithmetic shift right by one bit (2* shifts left one bit)
: bitwise ( a b -- )
  cr ." a = " over . ." b = " dup .
  cr ." a and b = " 2dup and .
  cr ." a  or b = " 2dup  or .
  cr ." a xor b = " 2dup xor .
  cr ." not a = " over invert .
  cr ." a shl b = " 2dup lshift .
  cr ." a shr b = " 2dup rshift .
  cr ." a ashr b = " 2dup arshift .
  2drop ;
