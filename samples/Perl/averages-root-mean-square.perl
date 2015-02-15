use v5.10.0;
sub rms
{
        my $r = 0;
        $r += $_**2 for @_;
        sqrt( $r/@_ );
}

say rms(1..10);
