sub writedat {
    my ($filename, $x, $y, $xprecision, $yprecision) = @_;
    open FH, ">", $filename or die "Can't open file: $!";
    printf FH "%.*g\t%.*g\n", $xprecision||3, $x->[$_], $yprecision||5, $y->[$_] for 0 .. $#$x;
    close FH;
}

my @x = (1, 2, 3, 1e11);
my @y = map sqrt, @x;

writedat("sqrt.dat", \@x, \@y);
