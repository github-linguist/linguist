: safe-/ ( x y -- x/y )
  ['] / catch -55 = if cr ." divide by zero!" 2drop 0 then ;
