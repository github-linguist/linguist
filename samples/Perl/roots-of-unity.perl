use Math::Complex;

foreach my $n (2 .. 10) {
  printf "%2d", $n;
  my @roots = root(1,$n);
  foreach my $root (@roots) {
    $root->display_format(style => 'cartesian', format => '%.3f');
    print " $root";
  }
  print "\n";
}
