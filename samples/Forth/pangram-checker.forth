: pangram? ( addr len -- ? )
  0 -rot bounds do
    i c@ 32 or [char] a -
    dup 0 26 within if
      1 swap lshift or
    else drop then
  loop
  1 26 lshift 1- = ;

s" The five boxing wizards jump quickly." pangram? .   \ -1
