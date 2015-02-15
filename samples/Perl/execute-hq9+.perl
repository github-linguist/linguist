#!/usr/bin/perl
use warnings;
use strict;
use feature qw(say switch);

my @programme = <> or die "No input. Specify a program file or pipe it to the standard input.\n";

for (@programme) {
    for my $char (split //) {
        given ($char) {
            when ('H') { hello()             }
            when ('Q') { quinne(@programme)  }
            when ('9') { bottles()           }
            default    { die "Unknown instruction $char.\n" } # Comment this line to ignore other instructions.
        }
    }
}

sub hello {
    print 'Hello World';
}

sub quinne {
    print @programme;
}

sub bottles {
    for my $n (reverse 0 .. 99) {
        my $before = bottle_count($n);
        my $after  = bottle_count($n - 1);
        my $action = bottle_action($n);
        say "\u$before of beer on the wall, $before of beer.";
        say "$action, $after of beer on the wall.";
        say q() if $n;
    }
}

sub bottle_count {
    my $n = shift;
    given ($n) {
        when    (-1) { return '99 bottles'      }
        when    (0)  { return 'no more bottles' }
        when    (1)  { return '1 bottle'        }
        default      { return "$n bottles"      }
    }
}

sub bottle_action {
    my $n = shift;
    return 'Take one down and pass it around' if $n > 0;
    return 'Go to the store and buy some more';
}
