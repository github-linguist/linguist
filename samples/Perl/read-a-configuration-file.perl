my $fullname;
my $favouritefruit;
my $needspeeling;
my $seedsremoved;
my @otherfamily;

# configuration file definition.  See read_conf_file below for explanation.
my $conf_definition = {
    'fullname'          => [ 'string', \$fullname ],
    'favouritefruit'    => [ 'string', \$favouritefruit ],
    'needspeeling'      => [ 'boolean', \$needspeeling ],
    'seedsremoved'      => [ 'boolean', \$seedsremoved ],
    'otherfamily'       => [ 'array', \@otherfamily ],
};

my $arg = shift;               # take the configuration file name from the command line
                               # (or first subroutine argument if this were in a sub)
my $file;                      # this is going to be a file handle reference
open $file, $arg or die "Can't open configuration file '$arg': $!";

read_conf_file($file, $conf_definition);

print "fullname = $fullname\n";
print "favouritefruit = $favouritefruit\n";
print "needspeeling = ", ($needspeeling ? 'true' : 'false'), "\n";
print "seedsremoved = ", ($seedsremoved ? 'true' : 'false'), "\n";
for (my $i = 0; $i < @otherfamily; ++$i) {
    print "otherfamily(", $i + 1, ") = ", $otherfamily[$i], "\n";
}

# read_conf_file:  Given a file handle opened for reading and a configuration definition,
# read the file.
# If the configuration file doesn't match the definition, raise an exception with "die".
# The configuration definition is (a reference to) an associative array
# where the keys are the configuration variable names in all lower case
# and the values are references to arrays.
# The first element of each of these arrays is the expected type:  'boolean', 'string', or 'array';
# the second element is a reference to the variable that should be assigned the data.
sub read_conf_file {
    my ($fh, $def) = @_;        # copy parameters

    local $_;                   # avoid interfering with use of $_ in main program
    while (<$fh>) {             # read a line from $fh into $_ until end of file
        next if /^#/;           # skip "#" comments
        next if /^;/;           # skip ";" comments
        next if /^$/;           # skip blank lines
        chomp;                  # strip final newline

        $_ =~ /^\s*(\w+)\s*(.*)$/i or die "Syntax error";
        my $key = $1;
        my $rest = $2;
        $key =~ tr/[A-Z]/[a-z]/; # convert keyword to lower case

        if (!exists $def->{$key}) {
            die "Unknown keyword: '$key'";
        }

        if ($def->{$key}[0] eq 'boolean') {
            if ($rest) {
                die "Syntax error:  extra data following boolean '$key'";
            }
            ${$def->{$key}[1]} = 1;
            next;                # done with this line, go back to "while"
        }

        $rest =~ s/\s*$//;       # drop trailing whitespace
        $rest =~ s/^=\s*//;      # drop equals sign if present

        if ($def->{$key}[0] eq 'string') {
            ${$def->{$key}[1]} = $rest;
        } elsif ($def->{$key}[0] eq 'array') {
            @{$def->{$key}[1]} = split /\s*,\s*/, $rest;
        } else {
            die "Internal error (unknown type in configuration definition)";
        }
    }
}
