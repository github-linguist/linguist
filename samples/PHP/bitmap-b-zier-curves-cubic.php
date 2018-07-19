<?

$image = imagecreate(200, 200);
// The first allocated color will be the background color:
imagecolorallocate($image, 255, 255, 255);
$color = imagecolorallocate($image, 255, 0, 0);
cubicbezier($image, $color, 160, 10, 10, 40, 30, 160, 150, 110);
imagepng($image);

function cubicbezier($img, $col, $x0, $y0, $x1, $y1, $x2, $y2, $x3, $y3, $n = 20) {
	$pts = array();

	for($i = 0; $i <= $n; $i++) {
		$t = $i / $n;
		$t1 = 1 - $t;
		$a = pow($t1, 3);
		$b = 3 * $t * pow($t1, 2);
		$c = 3 * pow($t, 2) * $t1;
		$d = pow($t, 3);

		$x = round($a * $x0 + $b * $x1 + $c * $x2 + $d * $x3);
		$y = round($a * $y0 + $b * $y1 + $c * $y2 + $d * $y3);
		$pts[$i] = array($x, $y);
	}

	for($i = 0; $i < $n; $i++) {
		imageline($img, $pts[$i][0], $pts[$i][1], $pts[$i+1][0], $pts[$i+1][1], $col);
	}
}
