my $string = "alphaBETA";
print uc($string), "\n"; # => "ALPHABETA"
print lc($string), "\n"; # => "alphabeta"
$string =~ tr/[a-z][A-Z]/[A-Z][a-z]/; print "$string\n"; # => ALPHAbeta

print ucfirst($string), "\n"; # => "AlphaBETA"
print lcfirst("FOObar"), "\n"; # => "fOObar"
