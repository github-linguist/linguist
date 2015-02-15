my $dec = "0123459";
my $hex_noprefix = "abcf123";
my $hex_withprefix = "0xabcf123";
my $oct_noprefix = "7651";
my $oct_withprefix = "07651";
my $bin_withprefix = "0b101011001";

print 0 + $dec, "\n";   # => 123459
print hex($hex_noprefix), "\n";    # => 180154659
print hex($hex_withprefix), "\n";    # => 180154659
print oct($hex_withprefix), "\n";    # => 180154659
print oct($oct_noprefix), "\n";    # => 4009
print oct($oct_withprefix), "\n";    # => 4009
print oct($bin_withprefix), "\n";    # => 345
# nothing for binary without prefix
