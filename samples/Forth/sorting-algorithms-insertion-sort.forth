: insert ( start end -- start )
  dup @ >r ( r: v )	\ v = a[i]
  begin
    2dup <			\ j>0
  while
    r@ over cell- @ <		\ a[j-1] > v
  while
    cell-			\ j--
    dup @ over cell+ !		\ a[j] = a[j-1]
  repeat then
  r> swap ! ;		\ a[j] = v

: sort ( array len -- )
  1 ?do dup i cells + insert loop drop ;

create test 7 , 3 , 0 , 2 , 9 , 1 , 6 , 8 , 4 , 5 ,
test 10 sort
test 10 cells dump
