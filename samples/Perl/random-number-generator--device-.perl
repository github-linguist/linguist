sub read_random {
        my $device = '/dev/urandom';
        open my $in, "<:raw", $device   # :raw because it's not unicode string
                or die "Can't open $device: $!";

        sysread $in, my $rand, 4 * shift;
        unpack('L*', $rand);
}

print "$_\n" for read_random(10);
