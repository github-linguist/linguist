sub spiral
 {my ($n, $x, $y, $dx, $dy, @a) = (shift, 0, 0, 1, 0);
  foreach (0 .. $n**2 - 1)
     {$a[$y][$x] = $_;
      my ($nx, $ny) = ($x + $dx, $y + $dy);
      ($dx, $dy) =
          $dx ==  1 && ($nx == $n || defined $a[$ny][$nx])
        ? ( 0,  1)
        : $dy ==  1 && ($ny == $n || defined $a[$ny][$nx])
        ? (-1,  0)
        : $dx == -1 && ($nx  <  0 || defined $a[$ny][$nx])
        ? ( 0, -1)
        : $dy == -1 && ($ny  <  0 || defined $a[$ny][$nx])
        ? ( 1,  0)
        : ($dx, $dy);
      ($x, $y) = ($x + $dx, $y + $dy);}
  return @a;}

foreach (spiral 5)
   {printf "%3d", $_ foreach @$_;
    print "\n";}
