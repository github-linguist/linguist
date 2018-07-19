use 5.10.0;
use strict;

{ # bracket state data into a lexical scope
	my @state;
	my $mod = 1_000_000_000;

	sub bentley_clever {
		my @s = ( shift() % $mod, 1);
		push @s, ($s[-2] - $s[-1]) % $mod	while @s < 55;
		@state = map($s[(34 + 34 * $_) % 55], 0 .. 54);
		subrand() for (55 .. 219);
	}

	sub subrand()
	{
		bentley_clever(0) unless @state; # just incase

		my $x = (shift(@state) - $state[-24]) % $mod;
		push @state, $x;
		$x;
	}
}

bentley_clever(292929);
say subrand() for (1 .. 10);
