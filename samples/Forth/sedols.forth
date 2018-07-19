create weight 1 , 3 , 1 , 7 , 3 , 9 ,

: char>num ( '0-9A-Z' -- 0..35 )
  dup [char] 9 > 7 and - [char] 0 - ;

: check+ ( sedol -- sedol' )
  6 <> abort" wrong SEDOL length"
  0 ( sum )
  6 0 do
    over I + c@ char>num
    weight I cells + @ *
    +
  loop
  10 mod   10 swap -  10 mod  [char] 0 +
  over 6 + c! 7 ;

: sedol"   [char] " parse check+ type ;

sedol" 710889" 7108899 ok
sedol" B0YBKJ" B0YBKJ7 ok
sedol" 406566" 4065663 ok
sedol" B0YBLH" B0YBLH2 ok
sedol" 228276" 2282765 ok
sedol" B0YBKL" B0YBKL9 ok
sedol" 557910" 5579107 ok
sedol" B0YBKR" B0YBKR5 ok
sedol" 585284" 5852842 ok
sedol" B0YBKT" B0YBKT7 ok
