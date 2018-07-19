: gcd ( a b -- n )
  begin dup while tuck mod repeat drop ;

: lcm ( a b -- n )
  over 0= over 0= or if 2drop 0 exit then
  2dup gcd abs */ ;
