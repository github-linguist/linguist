sub dice5 { 1+int rand(5) }

sub dice7 {
  while(1) {
    my $d7 = (5*dice5()+dice5()-6) % 8;
    return $d7 if $d7;
  }
}

my %count7;
my $n = 1000000;
$count7{dice7()}++ for 1..$n;
printf "%s: %5.2f%%\n", $_, 100*($count7{$_}/$n*7-1) for sort keys %count7;
