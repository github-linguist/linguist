sub a { print 'A'; return $_[0] }
sub b { print 'B'; return $_[0] }

# Test-driver
sub test {
    for my $op ('&&','||') {
        for (qw(1,1 1,0 0,1 0,0)) {
           my ($x,$y) = /(.),(.)/;
           print my $str = "a($x) $op b($y)", ': ';
           eval $str; print "\n"; } }
}

# Test and display
test();
