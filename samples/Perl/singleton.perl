package Singleton;
use strict;
use warnings;

my $Instance;

sub new {
    my $class = shift;
    $Instance ||= bless {}, $class; # initialised once only
}

sub name {
    my $self = shift;
    $self->{name};
}

sub set_name {
    my ($self, $name) = @_;
    $self->{name} = $name;
}

package main;

my $s1 = Singleton->new;
$s1->set_name('Bob');
printf "name: %s, ref: %s\n", $s1->name, $s1;

my $s2 = Singleton->new;
printf "name: %s, ref: %s\n", $s2->name, $s2;
