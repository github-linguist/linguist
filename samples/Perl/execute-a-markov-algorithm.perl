@ARGV == 1 or die "Please provide exactly one source file as an argument.\n";
open my $source, '<', $ARGV[0] or die "I couldn't open \"$ARGV[0]\" for reading. ($!.)\n";
my @rules;
while (<$source>)
   {/\A#/ and next;
    my @a = /(.*?)\s+->\s+(\.?)(.*)/ or die "Syntax error: $_";
    push @rules, \@a;}
close $source;

my $input = do {local $/; <STDIN>;};

OUTER:
   {foreach (@rules)
       {my ($from, $terminating, $to) = @$_;
        $input =~ s/\Q$from\E/$to/
            and ($terminating ? last OUTER : redo OUTER);}}

print $input;
