use XML::XPath qw();

my $x = XML::XPath->new('<inventory ... </inventory>');

[$x->findnodes('//item[1]')->get_nodelist]->[0];
print $x->findnodes_as_string('//price');
$x->findnodes('//name')->get_nodelist;
