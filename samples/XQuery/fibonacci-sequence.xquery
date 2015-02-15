declare function local:fib($n as xs:integer) as xs:integer {
  if($n < 2)
  then $n
  else local:fib($n - 1) + local:fib($n - 2)
};
