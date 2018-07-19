sub ltrim { shift =~ s/^\s+//r }
sub rtrim { shift =~ s/\s+$//r }
sub trim { ltrim rtrim shift }

# Usage:
my $p = "       this is a string      ";
print "'", $p, "'\n";
print "'", trim($p), "'\n";
print "'", ltrim($p), "'\n";
print "'", rtrim($p), "'\n";
