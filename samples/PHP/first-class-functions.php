$compose = function ($f, $g) {
    return function ($x) use ($f, $g) {
        return $f($g($x));
    };
};

$fn  = array('sin', 'cos', function ($x) { return pow($x, 3); });
$inv = array('asin', 'acos', function ($x) { return pow($x, 1/3); });

for ($i = 0; $i < 3; $i++) {
    $f = $compose($inv[$i], $fn[$i]);
    echo $f(0.5), PHP_EOL;
}
