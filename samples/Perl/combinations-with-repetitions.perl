sub p { $_[0] ? map p($_[0] - 1, [@{$_[1]}, $_[$_]], @_[$_ .. $#_]), 2 .. $#_ : $_[1] }
sub f { $_[0] ? $_[0] * f($_[0] - 1) : 1 }
sub pn{ f($_[0] + $_[1] - 1) / f($_[0]) / f($_[1] - 1) }

for (p(2, [], qw(iced jam plain))) {
        print "@$_\n";
}

printf "\nThere are %d ways to pick 7 out of 10\n", pn(7,10);
