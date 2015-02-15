my %node = (
     data => 'say what',
     next => \%foo_node,
     prev => \%bar_node,
);
$node{next} = \%quux_node;  # mutable
