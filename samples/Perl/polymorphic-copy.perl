package T;
sub new {
        my $cls = shift;
        bless [ @_ ], $cls
}

sub set_data {
        my $self = shift;
        @$self = @_;
}

sub copy {
        my $self = shift;
        bless [ @$self ], ref $self;
}

sub manifest {
        my $self = shift;
        print "type T, content: @$self\n\n";
}

package S;
our @ISA = 'T';
# S is inheriting from T.
# 'manifest' method is overriden, while 'new', 'copy' and
# 'set_data' are all inherited.
sub manifest {
        my $self = shift;
        print "type S, content: @$self\n\n";
}

package main;

print "# creating \$t as a T\n";
my $t = T->new('abc');
$t->manifest;

print "# creating \$s as an S\n";
my $s = S->new('SPQR');
$s->manifest;

print "# make var \$x as a copy of \$t\n";
my $x = $t->copy;
$x->manifest;

print "# now as a copy of \$s\n";
$x = $s->copy;
$x->manifest;

print "# show that this copy is indeed a separate entity\n";
$x->set_data('totally different');
print "\$x is: ";
$x->manifest;
