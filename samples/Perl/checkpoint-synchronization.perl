#!/usr/bin/perl
use warnings;
use strict;
use v5.10;

use Socket;

my $nr_items = 3;

sub short_sleep($) {
    (my $seconds) = @_;
    select undef, undef, undef, $seconds;
}

# This is run in a worker thread.  It repeatedly waits for a character from
# the main thread, and sends a value back to the main thread.  A short
# sleep introduces random timing, just to keep us honest.

sub be_worker($$) {
    my ($socket, $value) = @_;
    for (1 .. $nr_items) {
        sysread $socket, my $dummy, 1;
        short_sleep rand 0.5;
        syswrite $socket, $value;
        ++$value;
    }

    exit;
}

# This function forks a worker and sends it a socket on which to talk to
# the main thread, as well as an initial value to work with.  It returns
# (to the main thread) a socket on which to talk to the worker.

sub fork_worker($) {
    (my $value) = @_;
    socketpair my $kidsock, my $dadsock, AF_UNIX, SOCK_STREAM, PF_UNSPEC
        or die "socketpair: $!";

    if (fork // die "fork: $!") {
        # We're the parent
        close $dadsock;
        return $kidsock;
    }
    else {
        # We're the child
        close $kidsock;
        be_worker $dadsock, $value;
        # Never returns
    }
}

# Fork two workers, send them start signals, retrieve the values they send
# back, and print them

my $alpha_sock = fork_worker 'A';
my $digit_sock = fork_worker 1;

for (1 .. $nr_items) {
    syswrite $_, 'x'   for $alpha_sock, $digit_sock;
    sysread $alpha_sock, my $alpha, 1;
    sysread $digit_sock, my $digit, 1;
    say $alpha, $digit;
}

# If the main thread were planning to run for a long time after the
# workers had terminate, it would need to reap them to avoid zombies:

wait; wait;
