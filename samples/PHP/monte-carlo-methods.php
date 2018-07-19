<?
$loop = 1000000; # loop to 1,000,000
$count = 0;
for ($i=0; $i<$loop; $i++) {
  $x = rand() / getrandmax();
  $y = rand() / getrandmax();
  if(($x*$x) + ($y*$y)<=1) $count++;
}
echo "loop=".number_format($loop).", count=".number_format($count).", pi=".($count/$loop*4);
?>
