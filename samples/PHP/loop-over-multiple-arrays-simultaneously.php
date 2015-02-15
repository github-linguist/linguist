$a = array('a', 'b', 'c');
$b = array('A', 'B', 'C');
$c = array('1', '2', '3'); //These don't *have* to be strings, but it saves PHP from casting them later

if ((sizeOf($a) !== sizeOf($b)) || (sizeOf($b) !== sizeOf($c))){
  throw new Exception('All three arrays must be the same length');
}
foreach ($a as $key => $value){
  echo "{$a[$key]}{$b[$key]}{$c[$key]}\n";
}
