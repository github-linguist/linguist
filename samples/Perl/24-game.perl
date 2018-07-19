#!/usr/bin/perl

use strict;
use warnings;

print <<'EOF';
The 24 Game

Given any four digits in the range 1 to 9, which may have repetitions,
Using just the +, -, *, and / operators; and the possible use of
brackets, (), show how to make an answer of 24.

An answer of "q" will quit the game.
An answer of "!" will generate a new set of four digits.
Otherwise you are repeatedly asked for an expression until it evaluates to 24

Note: you cannot form multiple digit numbers from the supplied digits,
so an answer of 12+12 when given 1, 2, 2, and 1 would not be allowed.
EOF

while(1)
{
    my $iteration_num = 0;

    my $numbers = make_numbers();

    TRY_SOLVING:
    while(1)
    {
        $iteration_num++;
        print "Expression ${iteration_num}: ";

        my $entry = <>;
        chomp($entry);

        last TRY_SOLVING if $entry eq '!';

        exit if $entry eq 'q';

        my $result = play($numbers, $entry);

        if (!defined $result)
        {
            print "That's not valid\n";
            next TRY_SOLVING;
        }
        elsif ($result != 24)
        {
            print "Sorry, that's $result\n";
            next TRY_SOLVING;
        }
        else
        {
            print "That's right! 24!!\n";
            exit;
        }
    }
}

sub make_numbers
{
    my %numbers = ();

    print "Your four digits:";

    for(1..4)
    {
        my $i = 1 + int(rand(9));
        $numbers{$i}++;
        print "$i ";
    }

    print "\n";

    return \%numbers;
}

sub play
{
    my ($numbers, $expression) = @_;

    my %running_numbers = %$numbers;

    my @chars = split //, $expression;

    my $operator = 1;

    CHARS:
    foreach (@chars)
    {
        next CHARS if $_ =~ /[()]/;

        $operator = !$operator;

        if (! $operator)
        {
            if (defined $running_numbers{$_} && $running_numbers{$_} > 0)
            {
                $running_numbers{$_}--;
                next CHARS;
            }
            else
            {
                return;
            }
        }
        else
        {
            return if $_ !~ m{[-+*/]};
        }
    }

    foreach my $remaining (values(%running_numbers))
    {
        if ($remaining > 0)
        {
            return;
        }
    }

    return eval($expression);
}
