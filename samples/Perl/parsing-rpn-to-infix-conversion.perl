# RPN to infix conversion
#
# Nigel Galloway April 1st., 2012
#
$WSb = '(?:^|\s+)';
$WSa = '(?:\s+|$)';
$num = '([+-/$]?(?:\.\d+|\d+(?:\.\d*)?))';
$op = '([-+*/^])';
while (<>)  {
  $n = -1;
  while (s/$WSb$num\s+$num\s+$op$WSa/' '.('$'.++$n).' '/e)  {@elems[$n] = '('.$1.$3.$2.')';}
  while (s!(\$)(\d+)!@elems[$2]!e) {}
  print(substr($_,2,-2)."\n");
}
