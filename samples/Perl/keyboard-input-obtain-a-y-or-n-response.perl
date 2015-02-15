use Term::ReadKey;

ReadMode 4; # change to raw input mode

my $key = '';

while($key !~ /(Y|N)/i) {
    1 while defined ReadKey -1; # discard any previous input
    print "Type Y/N: ";
    $key = ReadKey 0; # read a single character
    print "$key\n";
}

ReadMode 0; # reset the terminal to normal mode

print "\nYou typed: $key\n";
