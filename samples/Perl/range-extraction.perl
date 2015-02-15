sub rangext {
    my $str = join ' ', @_;
    1 while $str =~ s{([+-]?\d+) ([+-]?\d+)}
        {$1.(abs($2 - $1) == 1 ? '~' : ',').$2}eg; # abs for neg ranges
    $str =~ s/(\d+)~(?:[+-]?\d+~)+([+-]?\d+)/$1-$2/g;
    $str =~ tr/~/,/;
    return $str;
}

# Test and display
my @test = qw(0  1  2  4  6  7  8 11 12 14,
             15 16 17 18 19 20 21 22 23 24,
             25 27 28 29 30 31 32 33 35 36,
             37 38 39);
print rangext(@test), "\n";
