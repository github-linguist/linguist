#!/usr/bin/perl

use strict;
use 5.10.0;

package Integrator;
use threads;
use threads::shared;

sub new {
	my $cls = shift;
	my $obj = bless {	t	=> 0,
				sum	=> 0,
				ref $cls ? %$cls : (),
				stop	=> 0,
				tid	=> 0,
				func	=> shift,
			}, ref $cls || $cls;

	share($obj->{sum});
	share($obj->{stop});

	$obj->{tid} = async {
		my $upd = 0.1; # update every 0.1 second
		while (!$obj->{stop}) {
			{
				my $f = $obj->{func};
				my $t = $obj->{t};

				$obj->{sum} += ($f->($t) + $f->($t + $upd))* $upd/ 2;
				$obj->{t} += $upd;
			}
			select(undef, undef, undef, $upd);
		}
	#	say "stopping $obj";
	};
	$obj
}

sub output { shift->{sum} }

sub delete {
	my $obj = shift;
	$obj->{stop} = 1;
	$obj->{tid}->join;
}

sub setinput {
	# This is surprisingly difficult because of the perl sharing model.
	# Func refs can't be shared, thus can't be replaced by another thread.
	# Have to create a whole new object... there must be a better way.
	my $obj = shift;
	$obj->delete;
	$obj->new(shift);
}

package main;

my $x = Integrator->new(sub { sin(atan2(1, 1) * 8 * .5 * shift) });

sleep(2);
say "sin after 2 seconds: ", $x->output;

$x = $x->setinput(sub {0});

select(undef, undef, undef, .5);
say "0 after .5 seconds: ", $x->output;

$x->delete;
