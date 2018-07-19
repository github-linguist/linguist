define('src_name', 'input.jpg');	// source image
define('dest_name', 'output.jpg');	// destination image

$img = imagecreatefromjpeg(src_name);	// read image

if(empty($img)){
	echo 'Image could not be loaded!';
	exit;
}

$black = imagecolorallocate($img, 0, 0, 0);
$white = imagecolorallocate($img, 255, 255, 255);
$width = imagesx($img);
$height = imagesy($img);

$array_lum = array(); 	// for storage of luminosity of each pixel
$sum_lum = 0;		// total sum of luminosity
$average_lum = 0;	// average luminosity of whole image

for($x = 0; $x < $width; $x++){	
	for($y = 0; $y < $height; $y++){
		// read pixel value
		$color = imagecolorat($img, $x, $y);
		$r = ($color >> 16) & 0xFF;
		$g = ($color >> 8) & 0xFF;
		$b = $color & 0xFF;
		// save pixel luminosity in temporary array
		$array_lum[$x][$y] = ($r + $g + $b);
		// add pixel luminosity to sum
		$sum_lum += $array_lum[$x][$y];
	}
}

// calculate average luminosity
$average_lum = $sum_lum / ($width * $height);

for($x = 0; $x < $width; $x++){	
	for($y = 0; $y < $height; $y++){
		// pixel is brighter than average -> set white
		// else -> set black
		if($array_lum[$x][$y] > $average_lum){
			imagesetpixel($img, $x, $y, $white);
		}
		else{
			imagesetpixel($img, $x, $y, $black);
		}
	}
}
// save black and white image to dest_name
imagejpeg($img, dest_name);

if(!file_exists(dest_name)){
	echo 'Image not saved! Check permission!';
}
