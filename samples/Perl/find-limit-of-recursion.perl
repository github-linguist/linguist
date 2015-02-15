my $x = 0;
recurse($x);

sub recurse ($x) {
   print ++$x,"\n";
   recurse($x);
}
