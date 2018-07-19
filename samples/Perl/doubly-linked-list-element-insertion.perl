my %node_model = (
        data => 'something',
        prev => undef,
        next => undef,
);

sub insert
{
        my ($anchor, $newlink) = @_;
        $newlink->{next} = $anchor->{next};
        $newlink->{prev} = $anchor;
        $newlink->{next}->{prev} = $newlink;
        $anchor->{next} = $newlink;
}

# create the list {A,B}
my $node_a = { %node_model };
my $node_b = { %node_model };

$node_a->{next} = $node_b;
$node_b->{prev} = $node_a;

# insert element C into a list {A,B}, between elements A and B.
my $node_c = { %node_model };
insert($node_a, $node_c);
