function random() {
    return mt_rand() / mt_getrandmax();
}

$pi 	= pi();          // Set PI

$a = array();
for ($i = 0; $i < 1000; $i++) {
    $a[$i] = 1.0 + ((sqrt(-2 * log(random())) * cos(2 * $pi * random())) * 0.5);

}
