package Example;
sub new {
    bless {}
}
sub foo {
    my ($self, $x) = @_;
    return 42 + $x;
}

package main;
my $name = "foo";
print Example->new->$name(5), "\n"; # prints "47"
