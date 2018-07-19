sub foo {
    foreach (0..1) {
        eval { bar($_) };
        if ($@ =~ /U0/) { print "Function foo caught exception U0\n"; }
        else { die; } # propagate the exception
    }
}

sub bar {
    baz(@_); # Nest those calls
}

sub baz {
    my $i = shift;
    die ($i ? "U1" : "U0");
}

foo();
