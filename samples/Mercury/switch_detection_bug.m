% This is a regression test for a bug in switch detection
% where it was preferring incomplete switches to complete
% one-case switches, and hence inferring the wrong determinism.

%------------------------------------------------------------------------------%

:- module switch_detection_bug.

:- interface.

:- type note ---> note(rank, modifier, octave).

:- type rank ---> c ; d ; e ; f ; g ; a ; b .

:- type modifier ---> natural ; sharp ; flat .

:- type octave == int.

:- type qualifier ---> maj ; min .

:- pred next_topnote(note, qualifier, note).
:- mode next_topnote(in, in, out) is multi.

%------------------------------------------------------------------------------%

:- implementation.

next_topnote(note(c, _, Oct), _, note(d, natural, Oct)).
next_topnote(note(d, _, Oct), _, note(c, natural, Oct)).
next_topnote(note(d, _, Oct), maj, note(e, natural, Oct)).
next_topnote(note(d, _, Oct), min, note(e, flat, Oct)).
next_topnote(note(e, _, Oct), _, note(d, natural, Oct)).
next_topnote(note(e, _, Oct), _, note(f, natural, Oct)).
next_topnote(note(f, _, Oct), maj, note(e, natural, Oct)).
next_topnote(note(f, _, Oct), min, note(e, flat, Oct)).
next_topnote(note(g, _, Oct), _, note(f, natural, Oct)).
next_topnote(note(g, _, Oct), min, note(a, flat, Oct)).
next_topnote(note(g, _, Oct), maj, note(a, natural, Oct)).
next_topnote(note(a, _, Oct), _, note(g, natural, Oct)).
next_topnote(note(a, _, Oct), min, note(b, flat, Oct)).
next_topnote(note(a, _, Oct), maj, note(b, natural, Oct)).
next_topnote(note(b, _, Oct), maj, note(a, natural, Oct)).
next_topnote(note(b, _, Oct), min, note(a, flat, Oct)).

%------------------------------------------------------------------------------%

