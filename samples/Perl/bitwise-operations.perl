use integer;

sub bitwise($$) {
   ($a, $b) = @_;
   print 'a and b: '. ($a & $b) ."\n";
   print 'a or b: '.  ($a | $b) ."\n";
   print 'a xor b: '. ($a ^ $b) ."\n";
   print 'not a: '.   (~$a)     ."\n";
   print 'a >> b: ', $a >> $b, "\n"; # logical right shift

   use integer; # "use integer" enables bitwise operations to return signed ints
   print "after use integer:\n";
   print 'a << b: ', $a << $b, "\n"; # left shift
   print 'a >> b: ', $a >> $b, "\n"; # arithmetic right shift
}
