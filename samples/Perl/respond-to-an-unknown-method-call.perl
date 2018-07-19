package Example;
sub new {
    bless {}
}
sub foo {
    print "this is foo\n";
}
sub bar {
    print "this is bar\n";
}
sub AUTOLOAD {
    my $name = $Example::AUTOLOAD;
    my ($self, @args) = @_;
    print "tried to handle unknown method $name\n";
    if (@args) {
        print "it had arguments: @args\n";
    }
}
sub DESTROY {}          # dummy method to prevent AUTOLOAD from
                        # being triggered when an Example is
                        # destroyed

package main;
my $example = Example->new;

$example->foo;          # prints "this is foo"
$example->bar;          # prints "this is bar"
$example->grill;        # prints "tried to handle unknown method Example::grill"
$example->ding("dong"); # prints "tried to handle unknown method Example::ding"
                        # and "it had arguments: dong"
