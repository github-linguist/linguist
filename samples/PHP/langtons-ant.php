// INIT AND DEFINITION
define('dest_name', 'output.png'); // destination image
define('width', 100);
define('height', 100);

$x = 50;
$y = 70;
$dir = 0; // 0-up, 1-left, 2-down, 3-right
$field = array();
$step_count = 0;

// LANGTON´S ANT PROCEDURE
while(0 <= $x && $x <= width && 0 <= $y && $y <= height){
	if(isset($field[$x][$y])){
		unset($field[$x][$y]);
		$dir = ($dir + 3) % 4;
	}else{
		$field[$x][$y] = true;
		$dir = ($dir + 1) % 4;
	}
	switch($dir){
		case 0: $y++; break;
		case 1: $x--; break;
		case 2: $y--; break;
		case 3: $x++; break;
	}
	$step_count++;
}
// ARRAY TO IMAGE
$img = imagecreatetruecolor(width, height);
$white = imagecolorallocate($img, 255, 255, 255);
for($x = 0; $x < width; $x++){	
	for($y = 0; $y < height; $y++){
		if(isset($field[$x][$y])){
			imagesetpixel($img, $x, $y, $white);
		}
	}
}
// TEXT TO IMAGE
$color = array();
$color[0] = imagecolorallocate($img, 255, 0, 0);
$color[1] = imagecolorallocate($img, 0, 255, 0);
$color[2] = imagecolorallocate($img, 0, 0, 255);
$print_array = array(
	0 => 'Langton`s Ant', 1=>'PHP Version', 2=>'Steps: ' . $step_count
);
foreach($print_array as $key => $line){
	imagestring($img, 3, 3, 3 + $key*11, $line, $color[$key]);
}
// SAVE IMAGE
imagepng($img, dest_name);
