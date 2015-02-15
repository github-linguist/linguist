use v5.16;  # ...for fc(), which does proper Unicode casefolding.
            # With older Perl versions you can use lc() as a poor-man's substitute.

sub compare {
    my ($a, $b) = @_;
    my $A = "'$a'";
    my $B = "'$b'";

    print "$A and $B are lexically equal.\n"     if $a eq $b;
    print "$A and $B are not lexically equal.\n" if $a ne $b;

    print "$A is lexically before $B.\n"         if $a lt $b;
    print "$A is lexically after $B.\n"          if $a gt $b;

    print "$A is not lexically before $B.\n"     if $a ge $b;
    print "$A is not lexically after $B.\n"      if $a le $b;

    print "The lexical relationship is: ", $a cmp $b, "\n";
    print "The case-insensitive lexical relationship is: ", fc($a) cmp fc($b), "\n";
    print "\n";
}

compare('Hello', 'Hello');
compare('5', '5.0');
compare('perl', 'Perl');
