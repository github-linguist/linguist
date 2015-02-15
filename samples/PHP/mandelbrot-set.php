$min_x=-2;
$max_x=1;
$min_y=-1;
$max_y=1;

$dim_x=400;
$dim_y=300;

$im = @imagecreate($dim_x, $dim_y)
  or die("Cannot Initialize new GD image stream");
header("Content-Type: image/png");
$black_color = imagecolorallocate($im, 0, 0, 0);
$white_color = imagecolorallocate($im, 255, 255, 255);

for($y=0;$y<=$dim_y;$y++) {
  for($x=0;$x<=$dim_x;$x++) {
    $c1=$min_x+($max_x-$min_x)/$dim_x*$x;
    $c2=$min_y+($max_y-$min_y)/$dim_y*$y;

    $z1=0;
    $z2=0;

    for($i=0;$i<100;$i++) {
      $new1=$z1*$z1-$z2*$z2+$c1;
      $new2=2*$z1*$z2+$c2;
      $z1=$new1;
      $z2=$new2;
      if($z1*$z1+$z2*$z2>=4) {
        break;
      }
    }
    if($i<100) {
      imagesetpixel ($im, $x, $y, $white_color);
    }
  }
}

imagepng($im);
imagedestroy($im);
