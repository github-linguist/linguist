/^(.).*(.)$/,$f{$1}{$_}=$2 for qw(
audino bagon baltoy banette bidoof braviary bronzor carracosta charmeleon
cresselia croagunk darmanitan deino emboar emolga exeggcute gabite
girafarig gulpin haxorus heatmor heatran ivysaur jellicent jumpluff kangaskhan
kricketune landorus ledyba loudred lumineon lunatone machamp magnezone mamoswine
nosepass petilil pidgeotto pikachu pinsir poliwrath poochyena porygon2
porygonz registeel relicanth remoraid rufflet sableye scolipede scrafty seaking
sealeo silcoon simisear snivy snorlax spoink starly tirtouga trapinch treecko
tyrogue vigoroth vulpix wailord wartortle whismur wingull yamask
);

sub poke
{
	my $h = $f{$_[0]};
	for my $w (keys %$h) {
		my $v = $h->{$w};
		delete $h->{$w};
		push @w, $w;
		@m = @w if @w > @m;
		poke($v);
		pop @w;
		$h->{$w} = $v;
	}
}

poke($_) for keys %f;
print @m.": @m\n";
