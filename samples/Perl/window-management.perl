#!perl
use strict;
use warnings;
use Tk;

my $mw;
my $win;
my $lab;

# How to open a window.
sub openWin {
	if( $win ) {
		$win->deiconify;
		$win->wm('state', 'normal');
	} else {
		eval { $win->destroy } if $win;
		$win = $mw->Toplevel;
		$win->Label(-text => "This is the window being manipulated")
			->pack(-fill => 'both', -expand => 1);
		$lab->configure(-text => "The window object is:\n$win");
	}
}

# How to close a window
sub closeWin {
	return unless $win;
	$win->destroy;
	$lab->configure(-text => '');
	undef $win;
}

# How to minimize a window
sub minimizeWin {
	return unless $win;
	$win->iconify;
}

# How to maximize a window
sub maximizeWin {
	return unless $win;
	$win->wm('state', 'zoomed');
	eval { $win->wmAttribute(-zoomed => 1) }; # Hack for X11
}

# How to move a window
sub moveWin {
	return unless $win;
	my ($x, $y) = $win->geometry() =~ /\+(\d+)\+(\d+)\z/ or die;
	$_ += 10 for $x, $y;
	$win->geometry("+$x+$y");
}

# How to resize a window
sub resizeWin {
	return unless $win;
	my ($w, $h) = $win->geometry() =~ /^(\d+)x(\d+)/ or die;
	$_ += 10 for $w, $h;
	$win->geometry($w . "x" . $h);
}

$mw = MainWindow->new;
for my $label0 ($mw->Label(-text => 'Window handle:')) {
	$lab = $mw->Label(-text => '');
	$label0->grid($lab);
}

my @binit = ('Open/Restore' => \&openWin, Close => \&closeWin,
	Minimize => \&minimizeWin, Maximize => \&maximizeWin,
	Move => \&moveWin, Resize => \&resizeWin);

while( my ($text, $callback) = splice @binit, 0, 2 ) {
	$mw->Button(-text => $text, -command => $callback)->grid('-');
}

MainLoop();

__END__
