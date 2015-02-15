sub preorder
{
	my $t = shift or return ();
	return ($t->[0], preorder($t->[1]), preorder($t->[2]));
}

sub inorder
{
	my $t = shift or return ();
	return (inorder($t->[1]), $t->[0], inorder($t->[2]));
}

sub postorder
{
	my $t = shift or return ();
	return (postorder($t->[1]), postorder($t->[2]), $t->[0]);
}

sub depth
{
	my @ret;
	my @a = ($_[0]);
	while (@a) {
		my $v = shift @a or next;
		push @ret, $v->[0];
		push @a, @{$v}[1,2];
	}
	return @ret;
}

my $x = [1,[2,[4,[7]],[5]],[3,[6,[8],[9]]]];

print "pre:   @{[preorder($x)]}\n";
print "in:    @{[inorder($x)]}\n";
print "post:  @{[postorder($x)]}\n";
print "depth: @{[depth($x)]}\n";
