function nthroot($number, $root, $p = P)
{
    $x[0] = $number;
    $x[1] = $number/$root;
    while(abs($x[1]-$x[0]) > $p)
    {
        $x[0] = $x[1];
        $x[1] = (($root-1)*$x[1] + $number/pow($x[1], $root-1))/$root;
    }
    return $x[1];
}
