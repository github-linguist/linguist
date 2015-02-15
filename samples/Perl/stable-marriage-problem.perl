#!/usr/bin/env perl
use strict;
use warnings;
use feature qw/say/;
use List::Util qw(first);

my %Likes = (
  M => {
    abe  => [qw/ abi eve cath ivy jan dee fay bea hope gay /],
    bob  => [qw/ cath hope abi dee eve fay bea jan ivy gay /],
    col  => [qw/ hope eve abi dee bea fay ivy gay cath jan /],
    dan  => [qw/ ivy fay dee gay hope eve jan bea cath abi /],
    ed   => [qw/ jan dee bea cath fay eve abi ivy hope gay /],
    fred => [qw/ bea abi dee gay eve ivy cath jan hope fay /],
    gav  => [qw/ gay eve ivy bea cath abi dee hope jan fay /],
    hal  => [qw/ abi eve hope fay ivy cath jan bea gay dee /],
    ian  => [qw/ hope cath dee gay bea abi fay ivy jan eve /],
    jon  => [qw/ abi fay jan gay eve bea dee cath ivy hope /],
  },

  W => {
    abi  => [qw/ bob fred jon gav ian abe dan ed col hal /],
    bea  => [qw/ bob abe col fred gav dan ian ed jon hal /],
    cath => [qw/ fred bob ed gav hal col ian abe dan jon /],
    dee  => [qw/ fred jon col abe ian hal gav dan bob ed /],
    eve  => [qw/ jon hal fred dan abe gav col ed ian bob /],
    fay  => [qw/ bob abe ed ian jon dan fred gav col hal /],
    gay  => [qw/ jon gav hal fred bob abe col ed dan ian /],
    hope => [qw/ gav jon bob abe ian dan hal ed col fred /],
    ivy  => [qw/ ian col hal gav fred bob abe ed jon dan /],
    jan  => [qw/ ed hal gav abe bob jon col ian fred dan /],
  },
);

my %Engaged;
my %Proposed;

match_them();
check_stability();
perturb();
check_stability();

sub match_them {
    say 'Matchmaking:';
    while(my $man = unmatched_man()) {
        my $woman = preferred_choice($man);
        $Proposed{$man}{$woman} = 1;
        if(! $Engaged{W}{$woman}) {
            engage($man, $woman);
            say "\t$woman and $man";
        }
        else {
            if(woman_prefers($woman, $man)) {
                my $engaged_man = $Engaged{W}{$woman};
                engage($man, $woman);
                undef $Engaged{M}{$engaged_man};
                say "\t$woman dumped $engaged_man for $man";
            }
        }
    }
}

sub check_stability {
    say 'Stablility:';
    my $stable = 1;
    foreach my $m (men()) {
        foreach my $w (women()) {
            if(man_prefers($m, $w) && woman_prefers($w, $m)) {
                say "\t$w prefers $m to $Engaged{W}{$w} and $m prefers $w to $Engaged{M}{$m}";
                $stable = 0;
            }
        }
    }
    if($stable) {
        say "\t(all marriages stable)";
    }
}

sub unmatched_man {
    return first { ! $Engaged{M}{$_} } men();
}

sub preferred_choice {
    my $man = shift;
    return first { ! $Proposed{$man}{$_} } @{ $Likes{M}{$man} };
}

sub engage {
    my ($man, $woman) = @_;
    $Engaged{W}{$woman} = $man;
    $Engaged{M}{$man} = $woman;
}

sub prefers {
    my $sex = shift;
    return sub {
        my ($person, $prospect) = @_;

        my $choices = join ' ', @{ $Likes{$sex}{$person} };
        return index($choices, $prospect) < index($choices, $Engaged{$sex}{$person});
    }
}

BEGIN {
    *woman_prefers = prefers('W');
    *man_prefers   = prefers('M');
}

sub perturb {
    say 'Perturb:';
    say "\tengage abi with fred and bea with jon";
    engage('fred' => 'abi');
    engage('jon'  => 'bea');
}

sub men   { keys %{ $Likes{M} } }
sub women { keys %{ $Likes{W} } }
