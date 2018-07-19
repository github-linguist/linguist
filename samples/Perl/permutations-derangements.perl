sub d {
        # compare this with the deranged() sub to see how to turn procedural
        # code into functional one ('functional' as not in 'understandable')
        $#_ ? map d([ @{$_[0]}, $_[$_] ], @_[1 .. $_-1, $_+1 .. $#_ ]),
                        grep { $_[$_] != @{$_[0]} } 1 .. $#_
            : $_[0]
}

sub deranged {  # same as sub d above, just a readable version to explain method
        my ($result, @avail) = @_;
        return $result if !@avail;              # no more elements to pick from, done

        my @list;                               # list of permutations to return
        for my $i (0 .. $#avail) {              # try to add each element to result in turn
                next if $avail[$i] == @$result; # element n at n-th position, no-no
                my $e = splice @avail, $i, 1;   # move the n-th element from available to result
                push @list, deranged([ @$result, $e ], @avail);
                                                # and recurse down, keep what's returned
                splice @avail, $i, 0, $e;       # put that element back, try next
        }
        return @list;
}

sub choose {    # choose k among n, i.e. n! / k! (n-k)!
        my ($n, $k) = @_;
        factorial($n) / factorial($k) / factorial($n - $k)
}

my @fact = (1);
sub factorial {
        # //= : standard caching technique.  If cached value available,
        #       return it; else compute, cache and return.
        #       For this specific task not really necessary.
        $fact[ $_[0] ] //= $_[0] * factorial($_[0] - 1)
}

my @subfact;
sub sub_factorial {
        my $n = shift;
        $subfact[$n] //= do     # same caching stuff, try comment out this line
        {
                # computes deranged without formula, using recursion
                my $total = factorial($n);      # total permutations
                for my $k (1 .. $n) {
                        # minus the permutations where k items are fixed
                        # to original location, and the rest deranged
                        $total -= choose($n, $k) * sub_factorial($n - $k)
                }
                $total
        }
}

print "Derangements for 4 elements:\n";
my @deranged = d([], 0 .. 3);
for (1 .. @deranged) {
        print "$_: @{$deranged[$_-1]}\n"
}

print "\nCompare list length and calculated table\n";
for (0 .. 9) {
        my @x = d([], 0 .. $_-1);
        print $_, "\t", scalar(@x), "\t", sub_factorial($_), "\n"
}

print "\nNumber of derangements:\n";
print "$_:\t", sub_factorial($_), "\n" for 1 .. 20;
