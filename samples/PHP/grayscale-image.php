class BitmapGrayscale extends Bitmap {
  public function toGrayscale(){
    for ($i = 0; $i < $this->h; $i++){
      for ($j = 0; $j < $this->w; $j++){
        $l = ($this->data[$j][$i][0] * 0.2126)
           + ($this->data[$j][$i][1] * 0.7152)
           + ($this->data[$j][$i][2] * 0.0722);
        $l = round($l);
        $this->data[$j][$i] = array($l,$l,$l);
      }
    }
  }
}

$b = new BitmapGrayscale(16,16);
$b->fill(0,0,null,null, array(255,255,0));
$b->setPixel(0, 15, array(255,0,0));
$b->setPixel(0, 14, array(0,255,0));
$b->setPixel(0, 13, array(0,0,255));
$b->toGrayscale();
$b->writeP6('p6-grayscale.ppm');
