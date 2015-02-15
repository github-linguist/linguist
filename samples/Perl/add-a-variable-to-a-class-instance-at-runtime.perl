package Empty;

# Constructor. Object is hash.
sub new { return bless {}, shift; }

package main;

# Object.
my $o = Empty->new;

# Set runtime variable (key => value).
$o->{'foo'} = 1;
