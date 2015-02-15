target("METHINKS IT IS LIKE A WEASEL").

rndAlpha(64, 32).     % Generate a single random character
rndAlpha(P, P).	      % 32 is a space, and 65->90 are upper case
rndAlpha(Ch) :- random(N), P is truncate(64+(N*27)), !, rndAlpha(P, Ch).

rndTxt(0, []).        % Generate some random text (fixed length)
rndTxt(Len, [H|T]) :- succ(L, Len), rndAlpha(H), !, rndTxt(L, T).

score([], [], Score, Score).   % Score a generated mutation (count diffs)
score([Ht|Tt], [Ht|Tp], C, Score) :- !, score(Tt, Tp, C, Score).
score([_|Tt], [_|Tp], C, Score) :- succ(C, N), !, score(Tt, Tp, N, Score).
score(Txt, Score, Target) :- !, score(Target, Txt, 0, Score).

mutate(_, [], []).             % mutate(Probability, Input, Output)
mutate(P, [H|Txt], [H|Mut]) :- random(R), R < P, !, mutate(P, Txt, Mut).
mutate(P, [_|Txt], [M|Mut]) :- rndAlpha(M), !, mutate(P, Txt, Mut).

weasel(Tries, _, _, mutation(0, Result)) :-               % No differences=success
	format('~w~4|:~w~3| - ~s\n', [Tries, 0, Result]).
weasel(Tries, Chance, Target, mutation(S, Value)) :-	    % output progress
	format('~w~4|:~w~3| - ~s\n', [Tries, S, Value]), !, % and call again
	weasel(Tries, Chance, Target, Value).
weasel(Tries, Chance, Target, Start) :-
	findall(mutation(S,M),  % Generate 30 mutations, select the best.
		(between(1, 30, _), mutate(Chance, Start, M), score(M,S,Target)),
		Mutations),     % List of 30 mutations and their scores
	sort(Mutations, [Best|_]), succ(Tries, N),
	!, weasel(N, Chance, Target, Best).
weasel :-  % Chance->probability for a mutation, T=Target, Start=initial text
	target(T), length(T, Len), rndTxt(Len, Start), Chance is 1 - (1/(Len+1)),
	!, weasel(0, Chance, T, Start).
