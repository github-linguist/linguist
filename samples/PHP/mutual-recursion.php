<?php
function F($n)
{
  if ( $n == 0 ) return 1;
  return $n - M(F($n-1));
}

function M($n)
{
  if ( $n == 0) return 0;
  return $n - F(M($n-1));
}

$ra = array();
$rb = array();
for($i=0; $i < 20; $i++)
{
  array_push($ra, F($i));
  array_push($rb, M($i));
}
echo implode(" ", $ra) . "\n";
echo implode(" ", $rb) . "\n";
?>
