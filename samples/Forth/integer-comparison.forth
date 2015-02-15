: compare-integers ( a b -- )
   2dup < if ." a is less than b" then
   2dup > if ." a is greater than b" then
        = if ." a is equal to b" then ;
