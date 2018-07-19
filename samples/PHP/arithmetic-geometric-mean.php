define('PRECISION', 13);

function agm($a0, $g0, $tolerance = 1e-10)
{
    // the bc extension deals in strings and cannot convert
    // floats in scientific notation by itself - hence
    // this manual conversion to a string
    $limit = number_format($tolerance, PRECISION, '.', '');
    $an    = $a0;
    $gn    = $g0;
    do {
        list($an, $gn) = array(
            bcdiv(bcadd($an, $gn), 2),
            bcsqrt(bcmul($an, $gn)),
        );
    } while (bccomp(bcsub($an, $gn), $limit) > 0);

    return $an;
}

bcscale(PRECISION);
echo agm(1, 1 / bcsqrt(2));
