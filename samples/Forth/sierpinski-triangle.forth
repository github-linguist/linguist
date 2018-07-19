: stars ( mask -- )
  begin
    dup 1 and if [char] * else bl then emit
    1 rshift  dup
  while space repeat drop ;

: triangle ( order -- )
  1 swap lshift   ( 2^order )
  1 over 0 do
    cr  over i - spaces  dup stars
    dup 2* xor
  loop 2drop ;

5 triangle
