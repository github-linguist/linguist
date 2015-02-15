use Carp 'cluck';

sub g {cluck 'Hello from &g';}
sub f {g;}

f;
