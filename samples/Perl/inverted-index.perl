use Set::Object 'set';

# given an array of files, returns the index
sub createindex
{
    my @files = @_;

    my %iindex;

    foreach my $file (@files)
    {
	open(F, "<", $file) or die "Can't read file $file: $!";
	while(<F>) {
            s/\A\W+//;
	    foreach my $w (map {lc} grep {length() >= 3} split /\W+/)
	    {
		if ( exists($iindex{$w}) )
		{
		    $iindex{$w}->insert($file);
		} else {
		    $iindex{$w} = set($file);
		}
	    }
	}
	close(F);
    }
    return %iindex;
}

# given an index, search for words
sub search_words_with_index
{
    my %idx = %{shift()};
    my @words = @_;
    my $res = set();

    foreach my $w (map {lc} @words)
    {
	$w =~ s/\W+//g;            # strip non-words chars
        length $w < 3 and next;
        exists $idx{$w} or return set();
        $res = $res->is_null
          ? set(@{$idx{$w}})
          : $res * $idx{$w};       # set intersection
    }
    return @$res;
}

# TESTING
# USAGE: invidx.pl the,list,of,words file1 file2 .. fileN
my @searchwords = split /,/, shift;
# first arg is a comma-separated list of words to search for
print "$_\n"
    foreach search_words_with_index({createindex(@ARGV)}, @searchwords);
