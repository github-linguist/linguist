use warnings;
use strict;

for my $testcase (
    0,   0xa,   123,   254,   255,   256,
    257, 65534, 65535, 65536, 65537, 0x1fffff,
    0x200000
  )
{
    my @vlq = vlq_encode($testcase);
    printf "%8s %12s %8s\n", $testcase, ( join ':', @vlq ), vlq_decode(@vlq);
}

sub vlq_encode {
    my @vlq;
    my $binary = sprintf "%s%b", 0 x 7, shift;
    $binary =~ s/(.{7})$//;
    @vlq = ( unpack 'H2', ( pack 'B8', '0' . $1 ) );
    while ( 0 + $binary ) {
        $binary =~ s/(.{7})$//;
        unshift @vlq, ( unpack 'H2', pack 'B8', '1' . $1 );
    }
    return @vlq;
}

sub vlq_decode {
    my $num;
    $num .= sprintf "%07b", hex(shift @_) & 0x7f while @_;
    return oct '0b' . $num;
}
