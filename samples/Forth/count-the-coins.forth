\ counting change (SICP section 1.2.2)

: table create does> swap cells + @ ;
table coin-value 0 , 1 , 5 , 10 , 25 , 50 ,

: count-change ( total coin -- n )
  over 0= if
    2drop 1
  else over 0< over 0= or if
    2drop 0
  else
    2dup coin-value - over recurse
    >r 1- recurse r> +
  then then ;

100 5 count-change .
