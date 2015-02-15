#!/usr/bin/perl
use strict;

my @trees = (
    # 0..2 are same
    [ 'd', [ 'c', [ 'a', 'b', ], ], ],
    [ [ 'd', 'c' ], [ 'a', 'b' ] ],
    [ [ [ 'd', 'c', ], 'a', ], 'b', ],
    # and this one's different!
    [ [ [ [ [ [ 'a' ], 'b' ], 'c', ], 'd', ], 'e', ], 'f' ],
);

for my $tree_idx (1 .. $#trees) {
    print "tree[",$tree_idx-1,"] vs tree[$tree_idx]: ",
           cmp_fringe($trees[$tree_idx-1], $trees[$tree_idx]), "\n";
}

sub cmp_fringe {
    my $ti1 = get_tree_iterator(shift);
    my $ti2 = get_tree_iterator(shift);
    while (1) {
        my ($L, $R) = ($ti1->(), $ti2->());
        next if defined($L) and defined($R) and $L eq $R;
        return "Same" if !defined($L) and !defined($R);
        return "Different";
    }
}

sub get_tree_iterator {
    my @rtrees = (shift);
    my $tree;
    return sub {
        $tree = pop @rtrees;
        ($tree, $rtrees[@rtrees]) = @$tree while ref $tree;
        return $tree;
    }
}
