sub menu
{
        my ($prompt,@array) = @_;
        return '' unless @array;

        print "  $_: $array[$_]\n" for(0..$#array);
        print $prompt;
        $n = <>;
        return $array[$n] if $n =~ /^\d+$/ and defined $array[$n];
        return &menu($prompt,@array);
}

@a = ('fee fie', 'huff and puff', 'mirror mirror', 'tick tock');
$prompt = 'Which is from the three pigs: ';

$a = &menu($prompt,@a);

print "You chose: $a\n";
