class Bitmap {
  public $data;
  public $w;
  public $h;
  public function __construct($w = 16, $h = 16){
    $white = array_fill(0, $w, array(255,255,255));
    $this->data = array_fill(0, $h, $white);
    $this->w = $w;
    $this->h = $h;
  }
  //Fills a rectangle, or the whole image with black by default
  public function fill($x = 0, $y = 0, $w = null, $h = null, $color = array(0,0,0)){
    if (is_null($w)) $w = $this->w;
    if (is_null($h)) $h = $this->h;
    $w += $x;
    $h += $y;
    for ($i = $y; $i < $h; $i++){
      for ($j = $x; $j < $w; $j++){
        $this->setPixel($j, $i, $color);
      }
    }
  }
  public function setPixel($x, $y, $color = array(0,0,0)){
    if ($x >= $this->w) return false;
    if ($x < 0) return false;
    if ($y >= $this->h) return false;
    if ($y < 0) return false;
    $this->data[$y][$x] = $color;
  }
  public function getPixel($x, $y){
    return $this->data[$y][$x];
  }
}

$b = new Bitmap(16,16);
$b->fill();
$b->fill(2, 2, 18, 18, array(240,240,240));
$b->setPixel(0, 15, array(255,0,0));
print_r($b->getPixel(3,3)); //(240,240,240)
