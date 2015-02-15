# the first four examples use regular expressions, so make sure to escape any special regex characters in the substring
"abcd" =~ /^ab/ #returns true
"abcd" =~ /zn$/ #returns false
"abab" =~ /bb/ #returns false
"abab" =~ /ab/ #returns true
my $loc = index("abab", "bb") #returns -1
$loc = index("abab", "ab") #returns 0
$loc = index("abab", "ab", $loc+1) #returns 2
