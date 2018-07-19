use strict;
use warnings;
use Tk;

sub get_size {
	my $mw = MainWindow->new();
	return ($mw->maxsize);
}
