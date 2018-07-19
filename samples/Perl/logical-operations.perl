sub show_bool
{
        return shift() ? 'true' : 'false', "\n";
}

sub test_logic
{
        my ($a, $b) = @_;
        print "a and b is ", show_bool $a && $b;
        print "a or b is ", show_bool $a || $b;
        print "not a is ", show_bool !$a;
        print "a xor b is ", show_bool($a xor $b);
}
