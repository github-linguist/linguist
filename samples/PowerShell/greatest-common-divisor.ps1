function Get-GCD ($x, $y)
{
  if ($x -eq $y) { return $y }
  if ($x -gt $y) {
    $a = $x
    $b = $y
  }
  else {
    $a = $y
    $b = $x
  }
  while ($a % $b -ne 0) {
    $tmp = $a % $b
    $a = $b
    $b = $tmp
  }
  return $b
}
