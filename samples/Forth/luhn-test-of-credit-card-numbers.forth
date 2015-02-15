: luhn ( addr len -- ? )
  0 >r over +             ( R: sum )
  begin  1- 2dup <=
  while                   \ odd
         dup c@ [char] 0 -
         r> + >r
         1- 2dup <=
  while                   \ even
         dup c@ [char] 0 -
         2* 10 /mod +     \ even digits doubled, split, and summed
         r> + >r
  repeat then
  2drop  r> 10 mod 0= ;

s" 49927398716"      luhn .   \ -1
s" 49927398717"      luhn .   \ 0
s" 1234567812345678" luhn .   \ 0
s" 1234567812345670" luhn .   \ -1
