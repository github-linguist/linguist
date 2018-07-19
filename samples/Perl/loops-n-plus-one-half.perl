foreach my $i (1..10) {
    print $i;
    last if $i == 10;
    print ', ';
}
print "\n";
