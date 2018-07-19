#! /usr/bin/perl
use strict;
my $trials = 10000;

my $stay = 0;
my $switch = 0;

foreach (1 .. $trials)
{
   my $prize = int(rand 3);
    # let monty randomly choose a door where he puts the prize
   my $chosen = int(rand 3);
    # let us randomly choose a door...
   my $show;
   do { $show = int(rand 3) } while $show == $chosen || $show == $prize;
    # ^ monty opens a door which is not the one with the
    # prize, that he knows it is the one the player chosen
   $stay++ if $prize == $chosen;
    # ^ if player chose the correct door, player wins only if he stays
   $switch++ if $prize == 3 - $chosen - $show;
    # ^ if player switches, the door he picks is (3 - $chosen - $show),
    # because 0+1+2=3, and he picks the only remaining door that is
    # neither $chosen nor $show
}

print "Stay win ratio " .  (100.0 * $stay/$trials) . "\n";
print "Switch win ratio " . (100.0 * $switch/$trials) . "\n";
