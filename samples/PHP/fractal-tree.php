<?php
header("Content-type: image/png");

$width = 512;
$height = 512;
$img = imagecreatetruecolor($width,$height);
$bg = imagecolorallocate($img,255,255,255);
imagefilledrectangle($img, 0, 0, $width, $width, $bg);

$depth = 8;
function drawTree($x1, $y1, $angle, $depth){

    global $img;

    if ($depth != 0){
        $x2 = $x1 + (int)(cos(deg2rad($angle)) * $depth * 10.0);
        $y2 = $y1 + (int)(sin(deg2rad($angle)) * $depth * 10.0);

        imageline($img, $x1, $y1, $x2, $y2, imagecolorallocate($img,0,0,0));

        drawTree($x2, $y2, $angle - 20, $depth - 1);
        drawTree($x2, $y2, $angle + 20, $depth - 1);
    }
}

drawTree($width/2, $height, -90, $depth);

imagepng($img);
imagedestroy($img);
?>
