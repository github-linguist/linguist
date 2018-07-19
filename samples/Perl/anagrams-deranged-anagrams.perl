sub deranged {                          # only anagrams ever get here
        my @a = split('', shift);       # split word into letters
        my @b = split('', shift);
        for (0 .. $#a) {
                $a[$_] eq $b[$_] and return;
        }
        return 1
}

sub find_deranged {
        for my $i ( 0 .. $#_ ) {
                for my $j ( $i+1 .. $#_ ) {
                        next unless deranged $_[$i], $_[$j];

                        print "length ", length($_[$i]), ": $_[$i] => $_[$j]\n";
                        return 1;
                }
        }
}

my %letter_list;
open my $in, 'unixdict.txt';

local $/ = undef;

for (split(' ', <$in>)) {
        # store anagrams in hash table by letters they contain
        push @{ $letter_list{ join('', sort split('', $_)) } }, $_
}

for (   sort { length($b) <=> length($a) }      # sort by length, descending
        grep { @{ $letter_list{$_} } > 1 }      # take only ones with anagrams
        keys %letter_list               )
{
        # if we find a pair, they are the longested due to the sort before
        last if find_deranged(@{ $letter_list{$_} });
}
