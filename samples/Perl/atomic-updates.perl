use strict;
use 5.10.0;

use threads 'yield';
use threads::shared;

my @a :shared = (100) x 10;
my $stop :shared = 0;

sub pick2 {
	my $i = int(rand(10));
	my $j;
	$j = int(rand(10)) until $j != $i;
	($i, $j)
}

sub even {
	lock @a;
	my ($i, $j) = pick2;
	my $sum = $a[$i] + $a[$j];
	$a[$i] = int($sum / 2);
	$a[$j] = $sum - $a[$i];
}

sub rand_move {
	lock @a;
	my ($i, $j) = pick2;

	my $x = int(rand $a[$i]);
	$a[$i] -= $x;
	$a[$j] += $x;
}

sub show {
	lock @a;
	my $sum = 0;
	$sum += $_ for (@a);
	printf "%4d", $_ for @a;
	print " total $sum\n";
}

my $t1 = async { even		until $stop }
my $t2 = async { rand_move	until $stop }
my $t3 = async {
	for (1 .. 10) {
		show;
		sleep(1);
	}
	$stop = 1;
};

$t1->join; $t2->join; $t3->join;
