use 5.010;
use strict;
use warnings;

my %prec = (
	'^' => 4,
	'*' => 3,
	'/' => 3,
	'+' => 2,
	'-' => 2,
	'(' => 1);

my %assoc = (
	'^' => 'right',
	'*' => 'left',
	'/' => 'left',
	'+' => 'left',
	'-' => 'left');

sub shunting_yard {
	my @inp = split ' ', $_[0];
	my @ops;
	my @res;

	my $report = sub { printf "%25s    %-7s %10s %s\n", "@res", "@ops", $_[0], "@inp" };
	my  $shift = sub { $report->( "shift @_"); push @ops, @_ };
	my $reduce = sub { $report->("reduce @_"); push @res, @_ };

	while( @inp ) {
		given( shift @inp ) {
			when ( /\d/ ) { $reduce->($_) }
			when ( '(' ) { $shift->($_) }
			when ( ')' ) { while( @ops and "(" ne (my $x = pop @ops) ) { $reduce->( $x ) } }
			default {
				my $newprec = $prec{$_};
				while( @ops ) {
					my $oldprec = $prec{$ops[-1]};
					last if $newprec > $oldprec;
					last if $newprec == $oldprec and $assoc{$_} eq 'right';
					$reduce->( pop @ops );
				}
				$shift->( $_ );
			}
		}
	}
    $reduce->( pop @ops ) while @ops;
    @res;
}

local $, = " ";
say shunting_yard '3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3' ;
