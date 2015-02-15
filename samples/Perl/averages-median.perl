sub median {
  my @a = sort {$a <=> $b} @_;
  return ($a[$#a/2] + $a[@a/2]) / 2;
}
