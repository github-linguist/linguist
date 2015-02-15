my $str = 'abcdefgh';
my $n = 2;
my $m = 3;
print substr($str, $n, $m), "\n";
print substr($str, $n), "\n";
print substr($str, 0, -1), "\n";
print substr($str, index($str, 'd'), $m), "\n";
print substr($str, index($str, 'de'), $m), "\n";
