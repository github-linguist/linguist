function char2value($c) {
  assert(stripos('AEIOU', $c) === FALSE);
  return intval($c, 36);
}

$sedolweight = array(1,3,1,7,3,9);

function checksum($sedol) {
    global $sedolweight;
    $tmp = array_sum(array_map(create_function('$ch, $weight', 'return char2value($ch) * $weight;'),
                               str_split($sedol), $sedolweight)
                    );
    return strval((10 - ($tmp % 10)) % 10);
}

foreach (array('710889',
               'B0YBKJ',
               '406566',
               'B0YBLH',
               '228276',
               'B0YBKL',
               '557910',
               'B0YBKR',
               '585284',
               'B0YBKT') as $sedol)
    echo $sedol, checksum($sedol), "\n";
