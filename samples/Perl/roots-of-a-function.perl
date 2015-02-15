sub f
{
        my $x = shift;

        return ($x * $x * $x - 3*$x*$x + 2*$x);
}

my $step = 0.001; # Smaller step values produce more accurate and precise results
my $start = -1;
my $stop = 3;
my $value = &f($start);
my $sign = $value > 0;

# Check for root at start

print "Root found at $start\n" if ( 0 == $value );

for(    my $x = $start + $step;
        $x <= $stop;
        $x += $step )
{
        $value = &f($x);

        if ( 0 == $value )
        {
                # We hit a root
                print "Root found at $x\n";
        }
        elsif ( ( $value > 0 ) != $sign )
        {
                # We passed a root
                print "Root found near $x\n";
        }

        # Update our sign
        $sign = ( $value > 0 );
}
