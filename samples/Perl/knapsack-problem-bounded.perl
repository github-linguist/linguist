#!/usr/bin/perl

use strict;

my $raw = <<'TABLE';
map     	9       150     1
compass 	13      35      1
water   	153     200     2
sandwich        50      60      2
glucose 	15      60      2
tin     	68      45      3
banana  	27      60      3
apple  		39      40      3
cheese  	23      30      1
beer    	52      10      1
suntancream     11      70      1
camera  	32      30      1
T-shirt 	24      15      2
trousers        48      10      2
umbrella        73      40      1
w_trousers     	42      70      1
w_overcoat  	43      75      1
note-case       22      80      1
sunglasses      7       20      1
towel   	18      12      2
socks   	4       50      1
book    	30      10      2
TABLE

my @items;
for (split "\n", $raw) {
        my @x = split /\s+/;
	push @items, {
		name	=> $x[0],
		weight	=> $x[1],
		value	=> $x[2],
		quant	=> $x[3],
	}
}

my $max_weight = 400;

my %cache;
sub pick {
	my ($weight, $pos) = @_;
	if ($pos < 0 or $weight <= 0) {
		return 0, 0, []
	}

	@{ $cache{$weight, $pos} //= [do{	# odd construct: for caching
		my $item = $items[$pos];
		my ($bv, $bi, $bw, $bp) = (0, 0, 0, []);

		for my $i (0 .. $item->{quant}) {
			last if $i * $item->{weight} > $weight;
			my ($v, $w, $p) = pick($weight - $i * $item->{weight}, $pos - 1);
			next if ($v += $i * $item->{value}) <= $bv;

			($bv, $bi, $bw, $bp) = ($v, $i, $w, $p);
		}

		my @picked = ( @$bp, $bi );
		$bv, $bw + $bi * $item->{weight}, \@picked
	}]}
}

my ($v, $w, $p) = pick(400, $#items);
for (0 .. $#$p) {
	if ($p->[$_] > 0) {
		print "$p->[$_] of $items[$_]{name}\n";
	}
}
print "Value: $v; Weight: $w\n";
