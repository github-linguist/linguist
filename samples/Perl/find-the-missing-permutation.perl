sub check_perm {
    my %hash; @hash{@_} = ();
    for my $s (@_) { exists $hash{$_} or return $_
        for map substr($s,1) . substr($s,0,1), (1..length $s); }
}

# Check and display
@perms = qw(ABCD CABD ACDB DACB BCDA ACBD ADCB CDAB DABC BCAD CADB CDBA
            CBAD ABDC ADBC BDCA DCBA BACD BADC BDAC CBDA DBCA DCAB);
print check_perm(@perms), "\n";
