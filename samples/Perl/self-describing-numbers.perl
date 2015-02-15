sub is_selfdesc
{
	local $_ = shift;
	my @b = (0) x length;
	$b[$_]++ for my @a = split //;
	return "@a" eq "@b";
}

# check all numbers from 0 to 100k plus two 'big' ones
for (0 .. 100000, 3211000, 42101000) {
	print "$_\n" if is_selfdesc($_);
}
