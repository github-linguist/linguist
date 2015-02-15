$i;
function sum (&$i, $lo, $hi, $term) {
    $temp = 0;
    for ($i = $lo; $i <= $hi; $i++) {
        $temp += $term();
    }
    return $temp;
}

echo sum($i, 1, 100, create_function('', 'global $i; return 1 / $i;')), "\n";
//Output: 5.18737751764 (5.1873775176396)

function sum ($lo,$hi)
{
 $temp = 0;
 for ($i = $lo; $i <= $hi; $i++)
 {
  $temp += (1 / $i);
 }
 return $temp;
}
echo sum(1,100);

//Output: 5.1873775176396
