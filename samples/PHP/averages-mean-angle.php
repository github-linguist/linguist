<?php
$samples = array(
	'1st' => array(350, 10),
	'2nd' => array(90, 180, 270, 360),
	'3rd' => array(10, 20, 30)
);

foreach($samples as $key => $sample){
	echo 'Mean angle for ' . $key . ' sample: ' . meanAngle($sample) . ' degrees.' . PHP_EOL;
}

function meanAngle ($angles){
	$y_part = $x_part = 0;
	$size = count($angles);
	for ($i = 0; $i < $size; $i++){
		$x_part += cos(deg2rad($angles[$i]));
		$y_part += sin(deg2rad($angles[$i]));
	}
	$x_part /= $size;
	$y_part /= $size;
	return rad2deg(atan2($y_part, $x_part));
}
?>
