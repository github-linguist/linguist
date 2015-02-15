<?php

class CatalanNumbersSerie
{
  private static $cache = array(0 => 1);

  private function fill_cache($i)
  {
    $accum = 0;
    $n = $i-1;
    for($k = 0; $k <= $n; $k++)
    {
      $accum += $this->item($k)*$this->item($n-$k);
    }
    self::$cache[$i] = $accum;
  }
  function item($i)
  {
    if (!isset(self::$cache[$i]))
    {
      $this->fill_cache($i);
    }
    return self::$cache[$i];
  }
}

$cn = new CatalanNumbersSerie();
for($i = 0; $i <= 15;$i++)
{
  $r = $cn->item($i);
  echo "$i = $r\r\n";
}
?>
