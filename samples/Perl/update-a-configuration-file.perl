use warnings;
use strict;

my $needspeeling         = 0;
my $seedsremoved         = 1;
my $numberofstrawberries = 62000;
my $numberofbananas      = 1024;
my $favouritefruit       = 'bananas';

my @out;

sub config {
    my (@config) = <DATA>;
    push @config, "NUMBEROFSTRAWBERRIES $numberofstrawberries\n"
        unless grep { /^;*[ \t]*NUMBEROFSTRAWBERRIES\b/; } @config;

    foreach my $line (@config) {

        if (substr($line, 0, 1) eq '#') {
            push @out, $line;
            next;
        }

        next if $line =~ /^[;\t ]+$/;

        my ($option, $option_name);

        if ($line =~ /^([A-Z]+[0-9]*)/) {
            $option_name = lc $1;
            $option      = eval "\\\$$option_name";
            my $value = eval "\${$option_name}";

            if ($value) {
                $$option = $value;
            }
            else {
                $line    = '; ' . $line;
                $$option = undef;
            }
        }
        elsif ($line =~ /^;+\s*([A-Z]+[0-9]*)/) {
            $option_name = lc $1;
            $option      = eval "\\\$$option_name";
            my $value = eval "\${$option_name}";

            if ($value) {
                $line =~ s/^;+\s*//;
                $$option = $value == 1 ? '' : $value;
            }
            else {
                $$option = undef;
            }
        }

        if (defined $$option) {
            push @out, "\U$option_name\E $$option\n"
                unless grep { /^\U$option_name\E\b/; } @out;
        }
        else {
            $line =~ s/\s*[^[:ascii:]]+\s*$//;
            $line =~ s/[[:cntrl:]\s]+$//;
            push(@out, "$line\n");
        }
    }
}

config();

my $file = join('', @out);
$file =~ s/\n{3,}/\n\n/g;

print $file;

__DATA__
# This is a configuration file in standard configuration file format
#
# Lines begininning with a hash or a semicolon are ignored by the application
# program. Blank lines are also ignored by the application program.

# The first word on each non comment line is the configuration option.
# Remaining words or numbers on the line are configuration parameter
# data fields.

# Note that configuration option names are not case sensitive. However,
# configuration parameter data is case sensitive and the lettercase must
# be preserved.

# This is a favourite fruit
FAVOURITEFRUIT banana

# This is a boolean that should be set
NEEDSPEELING

# This boolean is commented out
; SEEDSREMOVED

# How many bananas we have
NUMBEROFBANANAS 48
