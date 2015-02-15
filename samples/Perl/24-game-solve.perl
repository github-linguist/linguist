# Fischer-Krause ordered permutation generator
# http://faq.perl.org/perlfaq4.html#How_do_I_permute_N_e
sub permute (&@) {
		my $code = shift;
		my @idx = 0..$#_;
		while ( $code->(@_[@idx]) ) {
			my $p = $#idx;
			--$p while $idx[$p-1] > $idx[$p];
			my $q = $p or return;
			push @idx, reverse splice @idx, $p;
			++$q while $idx[$p-1] > $idx[$q];
			@idx[$p-1,$q]=@idx[$q,$p-1];
		}
	}

@formats = (
	'((%d %s %d) %s %d) %s %d',
	'(%d %s (%d %s %d)) %s %d',
	'(%d %s %d) %s (%d %s %d)',
	'%d %s ((%d %s %d) %s %d)',
	'%d %s (%d %s (%d %s %d))',
	);

# generate all possible combinations of operators
@op = qw( + - * / );
@operators = map{ $a=$_; map{ $b=$_; map{ "$a $b $_" }@op }@op }@op;

while(1)
{
	print "Enter four integers or 'q' to exit: ";
	chomp($ent = <>);
	last if $ent eq 'q';

	
	if($ent !~ /^[1-9] [1-9] [1-9] [1-9]$/){ print "invalid input\n"; next }

	@n = split / /,$ent;
	permute { push @numbers,join ' ',@_ }@n;

	for $format (@formats)
	{
		for(@numbers)
		{
			@n = split;
			for(@operators)
			{
				@o = split;
				$str = sprintf $format,$n[0],$o[0],$n[1],$o[1],$n[2],$o[2],$n[3];
				$r = eval($str);
				print "$str\n" if $r == 24;
			}
		}
	}
}
