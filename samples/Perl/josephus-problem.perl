my @prisoner = 0 .. 40;
my $k = 3;
until (@prisoner == 1) {
    push @prisoner, shift @prisoner for 1 .. $k-1;
    shift @prisoner;
}

print "Prisoner @prisoner survived.\n"
