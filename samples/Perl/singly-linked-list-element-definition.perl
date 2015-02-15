my %node = (
    data => 'say what',
    next => \%foo_node,
);
$node{next} = \%bar_node;  # mutable
